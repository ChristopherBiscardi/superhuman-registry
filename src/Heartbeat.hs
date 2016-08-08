{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Heartbeat where

import           Control.Exception
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Database.PostgreSQL.Simple
import           Servant

import           Config
import           Utils                      (tryRunPG)

type HealthCheckAPI = "health" :> Get '[JSON] NoContent

checkHealth :: App NoContent
checkHealth = do
  pgCheck <- tryRunPG $ flip query_ "select 1"
  case (pgCheck :: Either SomeException [Only Int]) of
    Left e -> throwError err500 -- TODO: Log this as an Error
    Right _ -> return NoContent

