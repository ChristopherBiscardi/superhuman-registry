{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module SR.Health where

import           Control.Exception
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Servant
import Hasql.Session (sql)
import           Config
import           Utils                      (runPG)

type HealthCheckAPI = "health" :> Get '[JSON] NoContent

-- | This will return 500 if there's a pg error
checkHealth :: App NoContent
checkHealth = do
  _ <- runPG $ sql "select 1"
  return NoContent

