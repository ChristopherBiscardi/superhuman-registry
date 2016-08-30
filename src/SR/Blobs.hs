{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module SR.Blobs where

import Servant
import Control.Monad.IO.Class
import SR.Routes
import Config (App)
import SR.Types
import Data.UUID.V4 (nextRandom)
import Data.UUID (UUID, toString)
import Katip.Core (Severity(..), logStr)
import Katip.Monadic


blobServer :: Namespace -> Name -> ServerT Blobs App
blobServer namespace' name' = digests
        :<|> uploadBlob namespace' name'
        :<|> withUUID
       where digests digest' = digestsServer namespace' name' digest'
             withUUID uuid' = uuidBlob namespace' name' uuid'
                         :<|> uuidBlob namespace' name' uuid'
                         :<|> uuidBlob namespace' name' uuid'
                         :<|> uuidBlob namespace' name' uuid'

uuidBlob :: Namespace -> Name -> UUID -> App NoContent
uuidBlob = undefined

blobTODO :: Namespace -> Name -> Digest -> App NoContent
blobTODO namespace' name' digest = undefined

uploadBlob :: Namespace -> Name -> App (Headers '[
    Header "Location" URI,
    Header "Range" String,
    Header "Docker-Upload-UUID" UUID
  ] NoContent)
uploadBlob namespace' name' = do
  uuid <- liftIO $ nextRandom
  let uploadAPI = Proxy :: Proxy ("v2" :> Capture "namespace" Namespace :> Capture "name" Name :> "blobs" :> "uploads" :> Capture "uuid" UUID :> Put '[JSON] NoContent)
      mkURI = safeLink api uploadAPI
      uri = mkURI namespace' name' uuid
      response = addHeader uri
        $ addHeader "0-0"
        $ addHeader uuid NoContent
  $(logTM) InfoS (logStr $ show $ getHeaders response)
  return response

digestsServer :: Namespace -> Name -> ServerT Digests App
digestsServer namespace' name' digest' = blobTODO namespace' name' digest'
                   :<|> blobTODO namespace' name' digest'
                   :<|> blobTODO namespace' name' digest'
