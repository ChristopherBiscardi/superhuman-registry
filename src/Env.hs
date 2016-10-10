{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Env where

import           Data.ByteString (ByteString)
import Data.Word (Word16)
import           GHC.Generics
import           System.Envy
import Network.URI

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

data Settings = Settings
  { srHostname     :: URI
  } deriving (Show, Generic)

defaultURI :: URI
defaultURI = URI "" (Just $ URIAuth "" "localhost" "") "" "" ""

-- | Orphan Instance
instance Var URI where
  toVar uri = uriToString (id) uri ""
  fromVar = parseURI

instance DefConfig Settings where
  defConfig = Settings defaultURI

instance FromEnv Settings where
  fromEnv = gFromEnvCustom Option {
                    dropPrefixCount = 0
                  , customPrefix = ""
          }
