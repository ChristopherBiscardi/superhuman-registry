{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module SR.Links where

import           Control.Monad.Reader (asks)
import           Network.URI          (relativeTo)
import           Servant
import           Servant.Server
import           Data.UUID (UUID)
import Data.Text (Text)

import           Config
import           Env                  (Settings (..))
import           SR.Routes            (api)
import           SR.Types

-- /v2/<reponame>
--type RepoRoot = 

-- | /v2/<name>/blobs/<digest>
digestEndpoint = Proxy :: Proxy (
   "v2" :> Capture "namespace" Namespace :> Capture "name" Name
        :> "blobs" :> Capture "digest" Digest :> Get '[JSON] NoContent
   )

mkDigestLink :: Namespace -> Name -> Digest -> App URI
mkDigestLink namespace' name' digest' = do
  settings <- asks acSettings
  let mkURI = safeLink api digestEndpoint
      uri = mkURI namespace' name' digest' `relativeTo` srHostname settings
  return uri

uploadEndpoint = Proxy :: Proxy (
  "v2" :> Capture "namespace" Namespace :> Capture "name" Name
       :> "blobs" :> "uploads" :> Capture "uuid" UUID :> Put '[JSON] NoContent
  )

type RepoName = Text
mkUploadLink :: UUID -> Namespace -> Name -> App URI
mkUploadLink uuid namespace' name' = do
  settings <- asks acSettings
  let mkURI = safeLink api uploadEndpoint
  return $ mkURI namespace' name' uuid `relativeTo` srHostname settings
