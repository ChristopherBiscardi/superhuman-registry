{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Initialization where

import qualified Hasql.Connection as C
import qualified Hasql.Pool       as P
import           System.Envy      (decodeEnv)

import           Env              (PGConfig (..))

-- | Get PG Connection  Info from Env
pgSettings :: IO P.Pool
pgSettings = do
  env <- decodeEnv :: IO (Either String PGConfig)
  case env of
    Left str -> fail str
    Right PGConfig{..} -> do
      let pgConfig = C.settings host port user password database
      pool <- P.acquire (4, 10, pgConfig)
      return pool
