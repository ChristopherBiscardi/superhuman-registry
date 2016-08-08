{-# LANGUAGE OverloadedStrings #-}
module Logging where

import           Katip
import           System.IO (stdout)

env :: Environment
env = "production"

initLogging :: IO LogEnv
initLogging = do
  logEnv <- initLogEnv "ms-users" env
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  return $ registerScribe "stdout" handleScribe logEnv
