{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Env where

import           Data.ByteString (ByteString)
import Data.Word (Word16)
import           GHC.Generics
import           System.Envy

data PGConfig = PGConfig
  { host     :: ByteString
  , port     :: Word16
  , user     :: ByteString
  , password :: ByteString
  , database :: ByteString
  } deriving (Show, Generic)

instance DefConfig PGConfig where
  defConfig = PGConfig "localhost" 5432 "" "" ""

instance FromEnv PGConfig where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0
                  , customPrefix = "PG"
          }
