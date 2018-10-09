{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Network.JsonRpc
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Little JSON-RPC 2.0 client.
-- Functions for implementing the client side of JSON-RPC 2.0.
-- See <http://www.jsonrpc.org/specification>.
--
module Network.Ethereum.Web3.JsonRpc (
    remote
  , MethodName
  , ServerUri
  , batchCall
  , Request(..)
  ) where

import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Applicative            ((<|>))
import           Control.Exception              (throwIO)
import           Control.Monad                  ((>=>))
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Reader     (ReaderT, ask)
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.Text                      (Text)
import           Data.Vector                    (fromList)
import           Network.HTTP.Client            (Manager,
                                                 RequestBody (RequestBodyLBS),
                                                 httpLbs, method, parseRequest,
                                                 requestBody, requestHeaders,
                                                 responseBody)

import Debug.Trace

-- | Name of called method.
type MethodName = Text

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
remote :: Remote a => MethodName -> a
remote n = remote_ (call . Array . fromList)
  where
    call = connection . encode . Request n 1
    connection body = do
        (uri, manager) <- ask
        request <- parseRequest uri
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST" }
        -- liftIO $ putStr "RPCRequest: "
        -- liftIO $ print body
        r <- liftIO $ responseBody <$> httpLbs request' manager
        -- liftIO $ putStr "RPCResponse: "
        -- liftIO $ print r
        pure r

-- mkRequest :: Remote a => MethodName -> a
-- batchWeb3 :: [a] -> [ReaderT (String, Manager) IO ByteString]
-- batchWeb3 = callRPC . (map mkRequest)

-- mkRequest :: (VarArgReq a) => MethodName -> a
-- mkRequest n = varReq_ (call . Array . fromList)
--   where
--     call = Request n 1

-- syncReq = Request
--     { rqMethod = "eth_syncing"
--     , rqId     = 1
--     , rqParams = Array $ fromList [] }

batchCall :: (FromJSON a) => [Request] -> Web3 [a]
batchCall reqs = Web3 $ do
    x <- callRPC reqs
    d <- case eitherDecode x of
        Left e -> error $ show e
        Right x -> pure x
    let bss = (d :: [Response])
    pure $ map (\(Right s)-> s) $ map p $ map rsResult bss
    where
        p c = do
            x <- c
            pure $ case fromJSON x of
                Success r -> r
                Error e -> error $ show e


callRPC :: [Request] -> ReaderT (String, Manager) IO ByteString
callRPC requests = (call . Array . fromList) $ map toJSON requests -- (zip [1..] requests)
    where
    call request = connection $ encode $ (\x-> trace (show x) x) $ request
    connection body = do
        (uri, manager) <- ask
        request <- parseRequest uri
        let request' = request
                        { requestBody = RequestBodyLBS body
                        , requestHeaders = [("Content-Type", "application/json")]
                        , method = "POST" }
        r <- liftIO $ responseBody <$> httpLbs request' manager
        liftIO $ print "callRPC"
        liftIO $ print r
        pure r

decodeResponse :: FromJSON a => ByteString -> IO a
decodeResponse = tryParse . eitherDecode
             >=> tryJsonRpc . rsResult
             >=> tryParse . eitherDecode . encode -- . (\x->trace (show x) x)
  where tryJsonRpc :: Either RpcError a -> IO a
        tryJsonRpc = either (throwIO . JsonRpcFail) return
        tryParse :: Either String a -> IO a
        tryParse = either (throwIO . ParserFail) return



class VarArgReq a where
    varReq_ :: ([Value] -> Request) -> a

instance (ToJSON a, VarArgReq b) => VarArgReq (a -> b) where
    varReq_ f x = varReq_ (\xs -> f (toJSON x : xs))


-- instance VarArgReq Request where
--     varReq_ f = Web3 ((liftIO . decodeResponse) =<< f [])

class Remote a where
    remote_ :: ([Value] -> ReaderT (ServerUri, Manager) IO ByteString) -> a

instance (ToJSON a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\xs -> f (toJSON x : xs))

instance FromJSON a => Remote (Web3 a) where
    remote_ f = Web3 ((liftIO . decodeResponse) =<< f [])

-- | JSON-RPC request.
data Request = Request { rqMethod :: !Text
                       , rqId     :: !Int
                       , rqParams :: !Value }

instance ToJSON Request where
    toJSON rq = object [ "jsonrpc" .= String "2.0"
                       , "method"  .= rqMethod rq
                       , "params"  .= rqParams rq
                       , "id"      .= rqId rq ]

-- | JSON-RPC response.
data Response = Response
  { rsResult :: !(Either RpcError Value)
  } deriving (Show)

instance FromJSON Response where
    parseJSON = withObject "JSON-RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")
