module Utils where

import Control.Monad.Reader
import Database.PostgreSQL.Simple
import Data.Pool
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Crypto.Hash (hash, hashlazy, Digest, SHA256)

import Config

runPG :: (Connection -> IO a) -> App a
runPG fn = do
  pool <- asks getPG
  thing <- liftIO $ withResource pool fn
  return thing

tryRunPG :: Exception b => (Connection -> IO a) -> App (Either b a)
tryRunPG fn = do
  pool <- asks getPG
  thing <- liftIO $ try $ withResource pool fn
  return thing

mkDigest :: ByteString -> Digest SHA256
mkDigest = hash

mkLazyDigest :: LB.ByteString -> Digest SHA256
mkLazyDigest = hashlazy
