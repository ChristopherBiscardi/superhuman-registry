{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Utils where

import           Control.Exception
import           Control.Monad.Reader
import           Crypto.Hash                (Digest, SHA256, hash, hashlazy)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Hasql.Pool                 as P
import Hasql.Pool (UsageError(..))
import           Hasql.Session
import           Katip
import           Servant                    (err500, throwError)


import           Config

runPG :: Session a -> App a
runPG action = do
  pool <- asks acPGPool
  res <- liftIO $ P.use pool action
  case res of
    Left usageError -> do
      case usageError of
        ConnectionError Nothing -> do
          $(logTM) CriticalS "Unable to access Postgres"
          throwError err500
        ConnectionError (Just str) -> do
          $(logTM) CriticalS $ ls str
          throwError err500
        SessionError (ClientError (Just str)) -> do
          $(logTM) CriticalS $ ls str
          throwError err500
        SessionError (ClientError Nothing) -> do
          $(logTM) CriticalS "Client Error"
          throwError err500
        -- | TODO: ServerErrors might be handle-able in handlers?
        SessionError (ResultError (ServerError code message details hint)) -> do
          $(logTM) CriticalS $ ls (show code ++ "  " ++ show message)
          throwError err500
        SessionError (ResultError (UnexpectedResult txt)) -> do
          $(logTM) CriticalS $ ls txt
          throwError err500
        SessionError (ResultError (RowError i EndOfInput)) -> do
          $(logTM) CriticalS "End of Input"
          throwError err500
        SessionError (ResultError (RowError i UnexpectedNull)) -> do
          $(logTM) CriticalS "UnexpectedNull"
          throwError err500
        SessionError (ResultError (RowError i (ValueError txt))) -> do
          $(logTM) CriticalS $ ls ("RowError: " ++ (show i) ++ "--" ++ (show txt))
          throwError err500
    Right v -> return v

mkDigest :: ByteString -> Digest SHA256
mkDigest = hash

mkLazyDigest :: LB.ByteString -> Digest SHA256
mkLazyDigest = hashlazy
