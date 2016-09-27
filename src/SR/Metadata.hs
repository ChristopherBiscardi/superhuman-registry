{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SR.Metadata where

import Servant
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash as CH  (Digest, SHA256)

import SR.Routes
import Config (App)
import SR.Types
import SR.Blobs (blobServer)
import Utils (mkDigest)

metadataServer :: ServerT Metadata App
metadataServer namespace' name' = getTags namespace' name'
                  :<|> manifestsServer namespace' name'
                  :<|> blobServer namespace' name'

manifestsServer :: Namespace -> Name -> ServerT Manifests App
manifestsServer namespace' name' ref' = getManifest namespace' name' ref'
  :<|> putManifest namespace' name' ref'
  :<|> deleteManifest namespace' name' ref'
  :<|> headManifest namespace' name' ref'


getManifest :: Namespace -> Name -> Ref -> App NoContent
getManifest namespace' name _ = do
  liftIO $ print "getManifest"
  return undefined

putManifest :: Namespace
            -> Name
            -> Ref
            -> (CH.Digest CH.SHA256, Manifest)
            -> App (Headers '[
    Header "Content-Length" Int,
    Header "Docker-Content-Digest" CDigest
    ] NoContent)
putManifest namespace' name ref' (digest, manifest) = do
  liftIO $ print "putManifest"
  liftIO $ print digest
  -- let h = show body'
  -- liftIO $ print h
  return $ addHeader 0
         $ addHeader (CDigest digest) NoContent

deleteManifest :: Namespace -> Name -> Ref -> App NoContent
deleteManifest namespace' name _ = do
  liftIO $ print "deleteManifest"
  return undefined

headManifest :: Namespace -> Name -> Ref -> App NoContent
headManifest namespace' name _ = do
  liftIO $ print "headManifest"
  return undefined

getTags :: Namespace -> Name -> App NoContent
getTags namespace' name' = undefined
