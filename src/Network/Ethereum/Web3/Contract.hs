{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      :  Network.Ethereum.Web3.Contract
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ethereum contract generalized interface, e.g. 'event' function
-- catch all event depend by given callback function type.
--
-- @
-- runWeb3 $ do
--     event "0x..." $ \(MyEvent a b c) ->
--         liftIO $ print (a + b * c))
-- @
--
-- In other case 'call' function used for constant calls (without
-- transaction creation and change state), and 'sendTx' function
-- like a 'call' but return no contract method return but created
-- transaction hash.
--
-- @
-- runweb3 $ do
--   x  <- call "0x.." Latest MySelector
--   tx <- sendTx "0x.." nopay $ MySelector2 (x + 2)
-- @
--
module Network.Ethereum.Web3.Contract (
    EventAction(..)
  , Event(..)
  , event
  , event'
  , eventMany'
  , Method(..)
  , call
  , sendTx
  , NoMethod(..)
  , nopay
  ) where

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               (Async)
import           Control.Exception                      (throwIO)
import           Control.Monad                          (forM, void, when)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT (..))
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.Machine
import           Data.Maybe                             (listToMaybe, mapMaybe)
import           Data.Monoid                            ((<>))
import           Data.Proxy                             (Proxy (..))
import qualified Data.Text                              as T
import           Data.Text.Lazy                         (toStrict)
import qualified Data.Text.Lazy.Builder                 as B
import qualified Data.Text.Lazy.Builder.Int             as B
import           Generics.SOP
import           GHC.TypeLits
import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Event
import           Network.Ethereum.Web3.Encoding.Generic
import qualified Network.Ethereum.Web3.Eth              as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Data.Machine.Plan



--------------------------------------------------------------------------------
-- * Event Streaming
--------------------------------------------------------------------------------

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)

-- | Contract event listener
class Event e where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Address -> Filter e

-- | run 'event\'' one block at a time.
event :: forall p i ni e .
         ( DecodeEvent i ni e
         , Event e
         )
      => Filter e
      -> (e -> ReaderT Change Web3  EventAction)
      -> Web3 (Async ())
event fltr handler = forkWeb3 $ event' fltr handler

-- | same as event, but does not immediately spawn a new thread.
event' :: forall p i ni e .
          ( DecodeEvent i ni e
          , Event e
          )
       => Filter e
       -> (e -> ReaderT Change Web3 EventAction)
       -> Web3 ()
event' fltr handler = eventMany' fltr 0 handler

-- | 'event\'' take s a filter, a window size, and a handler. It runs the handler
-- | over the results of 'eventLogs' results using 'reduceEventStream'. If no
-- | 'TerminateEvent' action is thrown and the toBlock is not yet reached,
-- | it then transitions to polling.
eventMany' :: forall p i ni e .
           ( DecodeEvent i ni e
           , Event e
           )
       => Filter e
       -> Integer
       -> (e -> ReaderT Change Web3 EventAction)
       -> Web3 ()
eventMany' fltr window handler = do
    start <- mkBlockNumber $ filterFromBlock fltr
    let initState = FilterStreamState { fssCurrentBlock = start
                                      , fssInitialFilter = fltr
                                      , fssWindowSize = window
                                      }
    mLastProcessedFilterState <- reduceEventStream (playLogs initState) handler
    case mLastProcessedFilterState of
      Nothing -> startPolling fltr {filterFromBlock = BlockWithNumber start}
      Just a@(act, lastBlock) -> do
        end <- mkBlockNumber . filterToBlock $ fltr
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling fltr {filterFromBlock = BlockWithNumber pollingFromBlock}
  where
    startPolling fltr = do
      filterId <- Eth.newFilter fltr
      let pollTo = filterToBlock fltr
      void $ reduceEventStream (pollFilter filterId pollTo) handler

-- | Effectively a mapM_ over the machine using the given handler.
reduceEventStream :: Monad m
                  => MachineT m k [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> m (Maybe (EventAction, BlockNumber))
reduceEventStream filterChanges handler = fmap listToMaybe . runT $
       filterChanges
    ~> autoM (processChanges handler)
    ~> asParts
    ~> runWhile (\(act, _) -> act /= TerminateEvent)
    ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges :: Monad m
                   => (a -> ReaderT Change m EventAction)
                   -> [FilterChange a]
                   -> m [(EventAction, BlockNumber)]
    processChanges handler changes = forM changes $ \FilterChange{..} -> do
                                       act <- flip runReaderT filterChangeRawChange $
                                            handler filterChangeEvent
                                       return (act, changeBlockNumber filterChangeRawChange)

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   }

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these
-- | 'Filter' objects.
playLogs :: forall p k i ni e.
            ( DecodeEvent i ni e
            , Event e
            )
         => FilterStreamState e
         -> MachineT Web3 k [FilterChange e]
playLogs s = filterStream s
          ~> autoM Eth.getLogs
          ~> mapping mkFilterChanges

-- | polls a filter from the given filterId until the target toBlock is reached.
pollFilter :: forall p i ni e s k.
              ( DecodeEvent i ni e
              , Event e
              )
           => FilterId
           -> DefaultBlock
           -> MachineT Web3 k [FilterChange e]
pollFilter fid end = construct $ pollPlan fid end
  where
    pollPlan :: FilterId -> DefaultBlock -> PlanT k [FilterChange e] Web3 ()
    pollPlan fid end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          lift $ Eth.uninstallFilter fid
          stop
        else do
          liftIO $ threadDelay 1000000
          changes <- lift $ Eth.getFilterChanges fid
          yield $ mkFilterChanges changes
          pollPlan fid end

mkFilterChanges :: forall i ni e.
                   ( Event e
                   , DecodeEvent i ni e
                   )
                => [Change]
                -> [FilterChange e]
mkFilterChanges cs =
  flip mapMaybe cs $ \c@Change{..} -> do
    x <- decodeEvent c
    return $ FilterChange c x

data FilterStreamState e =
  FilterStreamState { fssCurrentBlock  :: BlockNumber
                    , fssInitialFilter :: Filter e
                    , fssWindowSize    :: Integer
                    }


-- | `filterStream` is a machine which represents taking an initial filter
-- | over a range of blocks b1, ... bn (where bn is possibly `Latest` or `Pending`,
-- | but b1 is an actual `BlockNumber`), and making a stream of filter objects
-- | which cover this filter in intervals of size `windowSize`. The machine
-- | halts whenever the `fromBlock` of a spanning filter either (1) excedes the
-- | initial filter's `toBlock` or (2) is greater than the chain head's `BlockNumber`.
filterStream :: FilterStreamState e
             -> MachineT Web3 k (Filter e)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
  where
    filterPlan :: FilterStreamState e -> PlanT k (Filter e) Web3 (FilterStreamState e)
    filterPlan initialState@FilterStreamState{..} = do
      end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
      if fssCurrentBlock > end
        then stop
        else do
          let to' = newTo end fssCurrentBlock fssWindowSize
              filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                         , filterToBlock = BlockWithNumber to'
                                         }
          yield filter'
          filterPlan $ initialState { fssCurrentBlock = succ to' }
    succ :: BlockNumber -> BlockNumber
    succ (BlockNumber bn) = BlockNumber $ bn + 1
    newTo :: BlockNumber -> BlockNumber -> Integer -> BlockNumber
    newTo upper (BlockNumber current) window = min upper . BlockNumber $ current + window

-- | Coerce a 'DefaultBlock' into a numerical block number.
mkBlockNumber :: DefaultBlock -> Web3 BlockNumber
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest           -> return 0
  _                  -> Eth.blockNumber

--------------------------------------------------------------------------------
-- * Transactions and Calls
--------------------------------------------------------------------------------

class Method a where
  selector :: Proxy a -> T.Text

-- | 'sendTx' is used to submit a state changing transaction.
sendTx :: ( Generic a
          , GenericABIEncode (Rep a)
          , Method a
          )
       => Call
       -- ^ Call configuration
       -> a
       -- ^ method data
       -> Web3 TxHash
sendTx call (dat :: a) =
  let sel = selector (Proxy :: Proxy a)
  in Eth.sendTransaction (call { callData = Just $ sel <> genericToData dat })

-- | 'call' is used to call contract methods that have no state changing effects,
-- | or to call m
call :: ( Generic a
        , GenericABIEncode (Rep a)
        , Generic b
        , GenericABIDecode (Rep b)
        , Method a
        )
     => Call
     -- ^ Call configuration
     -> DefaultBlock
     -- ^ State mode for constant call (latest or pending)
     -> a
     -- ^ Method data
     -> Web3 b
     -- ^ 'Web3' wrapped result
call call mode (dat :: a) = do
    let sel = selector (Proxy :: Proxy a)
    res <- Eth.call (call { callData = Just $ sel <> genericToData dat }) mode
    case genericFromData (T.drop 2 res) of
        Nothing -> liftIO $ throwIO $ ParserFail $
            "Unable to parse result on `" ++ T.unpack res
            ++ "` from `" ++ show (callTo call) ++ "`"
        Just x -> return x

-- | Zero value is used to send transaction without money
nopay :: Wei
{-# INLINE nopay #-}
nopay = 0

-- | Dummy method for sending transaction without method call
data NoMethod = NoMethod
