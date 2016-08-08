{-# LANGUAGE OverloadedStrings #-}

module Initialization where

import           Control.Monad               (liftM)
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Pool
import           Database.PostgreSQL.Simple
import           System.Environment
import qualified System.Posix.Env.ByteString as EB (getEnv)

-- | Get Connection Information from the environment, causing an error
--   if the env var isn't set and doesn't have a default value
getConnInfoFromEnv :: IO ConnectInfo
getConnInfoFromEnv = do
  host <- getEnv "POSTGRES_HOST"
  envPort <- lookupEnv "POSTGRES_PORT"
  let port = case envPort of
        Just str -> read str
        Nothing -> 5432
  user <- getEnv "POSTGRES_USER"
  password <- getEnv "POSTGRES_PASSWORD"
  dbName <- getEnv "POSTGRES_DATABASE_NAME"
  return ConnectInfo { connectHost=host
                     , connectPort=port
                     , connectUser=user
                     , connectPassword=password
                     , connectDatabase=dbName
                     }

data PGPoolConfig = PGPoolConfig { pgpcStripes   :: Int
                                 , pgpcIdleTime  :: Int
                                 , pgpcResources :: Int
                                 }

getEnvWithDefault i str = liftM (fromMaybe i . liftM read) (lookupEnv str)

getPoolCfgFromEnv :: IO PGPoolConfig
getPoolCfgFromEnv = do
  pgsNumStripes <- getEnvWithDefault 2 "POSTGRES_POOL_STRIPES"
  pgsIdleTime <- getEnvWithDefault 5 "POSTGRES_POOL_IDLE_TIME"
  pgsResources <- getEnvWithDefault 20 "POSTGRES_POOL_RESOURCES"
  return $ PGPoolConfig pgsNumStripes pgsIdleTime pgsResources


initPG :: IO (Pool Connection)
initPG = do
  connInfo <- getConnInfoFromEnv
  poolCfg <- getPoolCfgFromEnv
  pool <- liftIO $ createPool (connect connInfo) close
                              (pgpcStripes poolCfg)
                              (realToFrac $ pgpcIdleTime poolCfg)
                              (pgpcResources poolCfg)
  return pool
