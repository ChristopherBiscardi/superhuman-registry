{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module SR.Blobs where

import           Config                 (App)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString, writeFile)
import           Data.Maybe             (fromJust)
import           Data.UUID              (UUID, toString)
import           Data.UUID              (toString)
import           Data.UUID.V4           (nextRandom)
import           Katip.Core             (Severity (..), logStr)
import           Katip.Monadic
import           Network.URI            (parseURI, relativeTo)
import           Servant
import           SR.Routes
import           SR.Types
import Hasql.LO ()

blobServer :: Namespace -> Name -> ServerT Blobs App
blobServer namespace' name' = digests
        :<|> uploadBlob namespace' name'
        :<|> withUUID
       where digests digest' = digestsServer namespace' name' digest'
             withUUID uuid' = getBlob namespace' name' uuid'
                         :<|> patchBlob namespace' name' uuid'
                         :<|> putBlob namespace' name' uuid'
                         :<|> deleteBlob namespace' name' uuid'

getBlob :: Namespace -> Name -> UUID -> App NoContent
getBlob namespace' name' uuid' = do
  liftIO $ print "getBlob"
  return undefined

putBlob :: Namespace -> Name -> UUID -> Maybe Digest -> App NoContent
putBlob namespace' name' uuid' digest' = do
  liftIO $ print "putBlob"
  case digest' of
    Nothing -> return NoContent
    -- TODO: mv uuid file into sha256 location
    Just a -> return NoContent

deleteBlob :: Namespace -> Name -> UUID -> App NoContent
deleteBlob namespace' name' uuid' = do
  liftIO $ print "deleteBlob"
  return undefined

patchBlob :: Namespace
          -> Name
          -> UUID
          -> ByteString
          -> Maybe String
          -> App (Headers '[
    Header "Location" URI,
    Header "Range" String,
    Header "Docker-Upload-UUID" UUID
  ] NoContent)
patchBlob namespace' name' uuid' blob range' = do
  liftIO $ print range'
  liftIO $ Data.ByteString.writeFile ("./tmp/" ++ toString uuid') blob
  response <- mkHeaders range' uuid' namespace' name'
  return response

headDigest :: Namespace
           -> Name
           -> Digest
           -> App (Headers '[
    Header "Content-Length" Int,
    Header "Docker-Content-Digest" Digest
    ] NoContent)
headDigest namespace' name' digest' = do
  return $ addHeader 0
         $ addHeader digest' NoContent

getDigest :: Namespace -> Name -> Digest -> App NoContent
getDigest namespace' name' digest = do
  liftIO $ print "getDigest"
  return undefined

deleteDigest :: Namespace -> Name -> Digest -> App NoContent
deleteDigest namespace' name' digest = do
  liftIO $ print "deleteDigest"
  return undefined


uploadBlob :: Namespace -> Name -> App (Headers '[
    Header "Location" URI,
    Header "Range" String,
    Header "Docker-Upload-UUID" UUID
  ] NoContent)
uploadBlob namespace' name' = do
  uuid <- liftIO $ nextRandom
  response <- mkHeaders Nothing uuid namespace' name'
  $(logTM) InfoS (logStr $ show $ getHeaders response)
  return response

digestsServer :: Namespace -> Name -> ServerT Digests App
digestsServer namespace' name' digest' = headDigest namespace' name' digest'
                   :<|> getDigest namespace' name' digest'
                   :<|> deleteDigest namespace' name' digest'

mkHeaders :: Maybe String
          -> UUID
          -> Namespace
          -> Name
          -> App (Headers '[
                 Header "Location" URI,
                 Header "Range" String,
                 Header "Docker-Upload-UUID" UUID
                 ] NoContent)
mkHeaders range uuid namespace' name' = do
  let uploadAPI = Proxy :: Proxy ("v2" :> Capture "namespace" Namespace :> Capture "name" Name :> "blobs" :> "uploads" :> Capture "uuid" UUID :> Put '[JSON] NoContent)
      mkURI = safeLink api uploadAPI
      -- TODO: move parseURI into config
      uri = mkURI namespace' name' uuid `relativeTo` (fromJust $ parseURI "http://localhost:9000/")
  return $ addHeader uri
         $ addHeader (case range of
                         Nothing -> "0-0"
                         Just v -> v)
         $ addHeader uuid NoContent
