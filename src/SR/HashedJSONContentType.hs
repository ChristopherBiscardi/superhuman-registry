{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module SR.HashedJSONContentType where

import           Crypto.Hash                (Digest, SHA256)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Network.HTTP.Media         hiding (Accept)
import           Servant.API.ContentTypes

import           Utils                      (mkDigest)

data HashedJSON = HashedJSON String

instance Accept HashedJSON where
  contentType _ = "application" // "json"

-- instance Show a => MimeRender HashedJSON a where
--    mimeRender _ val = pack ("This is MINE! " ++ show val)

instance FromJSON a => MimeUnrender HashedJSON (Digest SHA256, a) where
   mimeUnrender _ bs = case eitherDecodeLenient bs of
     Left err -> Left err
     Right val -> Right (mkDigest $ BSC.toStrict bs, val)
