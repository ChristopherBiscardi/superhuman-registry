{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module SR.Blobs where

import           Config                 (App)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Data.ByteString        (ByteString, writeFile)
import qualified Data.ByteString        as B
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import           Data.UUID              (UUID, toString)
import           Data.UUID              (toString)
import           Data.UUID.V4           (nextRandom)
import           Katip.Core             (Severity (..), logStr)
import           Katip.Monadic
import           Network.URI            (parseURI, relativeTo)
import           Servant

import           Backend                (headBlob, receivePushContent,
                                         startNewUpload, registerBlob)
import           Backend.Types          (BlobExistance (..))
import           Config                 (AppConfig (..))
import           Env                    (Settings (..))
import           SR.Routes
import           SR.Types
import SR.Links (mkUploadLink)

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

putBlob :: Namespace
        -> Name
        -> UUID
        -> Maybe Digest
        -> ByteString
        -> App NoContent
putBlob namespace'@(Namespace ns') name@(Name n') uuid' digest' blob' = do
  liftIO $ print "putBlob"
  let hasBlob = B.length blob' /= 0
  case hasBlob of
    -- | If there is a blob here, it is the whole blob (and thus
    --   requires a new "blob_upload"
    True -> undefined
    False ->
      case digest' of
        -- | No Digest means it is an "UNSUPPORTED" operation
        --   TODO: Error Handling should be JSON
        Nothing -> throwError err400 { errBody = "Unsupported due to invalid set of parameters" }
        Just d -> do
          -- | TODO: Check digest/uuid against `blob_uploads`
          let reponame = T.intercalate "/" [ns', n']
          _ <- registerBlob reponame uuid' d
          -- | Location: /v2/<name>/blobs/<digest>
          return NoContent

deleteBlob :: Namespace -> Name -> UUID -> App NoContent
deleteBlob namespace' name' uuid' = do
  liftIO $ print "deleteBlob"
  return undefined

patchBlob :: Namespace
          -> Name
          -> UUID
          -> ByteString
          -> Maybe Range
          -> App (Headers '[
    Header "Location" URI,
    Header "Content-Range" Range,
    Header "Docker-Upload-UUID" UUID
  ] NoContent)
patchBlob namespace'@(Namespace ns') name'@(Name n') uuid' blob range' = do
  liftIO $ print range'
  let reponame = T.intercalate "/" [ns', n']
  _ <- receivePushContent range' blob uuid' reponame
  response <- mkHeaders range' uuid' namespace' name'
  return response

-- | TODO: Check if the semantics rely on reponame or this is just
-- a misplaced "blob check"
headDigest :: Namespace
           -> Name
           -> Digest
           -> App (Headers '[
    Header "Content-Length" Int,
    Header "Docker-Content-Digest" Digest
    ] NoContent)
headDigest namespace' name' digest' = do
  blobExists <- headBlob digest'
  case blobExists of
    BLOB_EXISTS -> return
      $ addHeader 0
      $ addHeader digest' NoContent
    UNKNOWN_BLOB -> throwError err404

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
    Header "Content-Range" Range,
    Header "Docker-Upload-UUID" UUID
  ] NoContent)
uploadBlob namespace'@(Namespace ns') name'@(Name n') = do
  uuid <- startNewUpload $ T.intercalate "/" [ns', n']
  uri <- mkUploadLink uuid namespace' name'
  return $ addHeader uri
         $ addHeader (Range 0 0)
         $ addHeader uuid NoContent

digestsServer :: Namespace -> Name -> ServerT Digests App
digestsServer namespace' name' digest' = headDigest namespace' name' digest'
                   :<|> getDigest namespace' name' digest'
                   :<|> deleteDigest namespace' name' digest'

mkHeaders :: Maybe Range
          -> UUID
          -> Namespace
          -> Name
          -> App (Headers '[
                 Header "Location" URI,
                 Header "Content-Range" Range,
                 Header "Docker-Upload-UUID" UUID
                 ] NoContent)
mkHeaders range uuid namespace' name' = do
  settings <- asks acSettings
  let uploadAPI = Proxy :: Proxy ("v2" :> Capture "namespace" Namespace :> Capture "name" Name :> "blobs" :> "uploads" :> Capture "uuid" UUID :> Put '[JSON] NoContent)
      mkURI = safeLink api uploadAPI
      uri = mkURI namespace' name' uuid `relativeTo` srHostname settings
  return $ addHeader uri
         $ addHeader (case range of
                         Nothing -> Range 0 0
                         Just range' -> range')
         $ addHeader uuid NoContent
