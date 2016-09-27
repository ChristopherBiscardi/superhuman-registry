{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module SR.Routes where

import qualified Crypto.Hash                   as CH (Digest, SHA256)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Builder       (lazyByteString)
import           Data.ByteString.Conversion.To (ToByteString (..))
import           Data.ByteString.Lazy.Char8    (pack)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.IO
import           Data.UUID                     (UUID, fromText, toASCIIBytes,
                                                toLazyASCIIBytes, toText)
import           Servant

import           SR.HashedJSONContentType
import           SR.Types
--import SR.Combinators.CaptureUntilInstances

api :: Proxy API
api = Proxy

type Head = Verb 'HEAD 200
type PutCreated = Verb 'PUT 201

type V2Base = "v2" :> Get '[JSON] (Headers '[
  Header "Docker-Distribution-API-Version" String
  ] NoContent)

-- | Main API Type
type API = V2Base :<|> "v2" :> V2API

-- | V2 API Definition
type V2API = Metadata
  :<|> "_catalog" :> Get '[JSON] NoContent

type Tags = "list" :> Get '[JSON] NoContent

type Metadata = Capture "namespace" Namespace :> Capture "name" Name :> (
  "tags" :> Tags :<|>
  "manifests" :> Manifests :<|>
  "blobs" :> Blobs
  )

type Blobs = Digests :<|> Upload

type Manifests = Capture "reference" Ref :> (
  Get '[JSON] NoContent :<|>
  ReqBody '[HashedJSON] (CH.Digest CH.SHA256, Manifest) :>
    PutCreated '[JSON] (Headers '[
      Header "Content-Length" Int,
      Header "Docker-Content-Digest" CDigest
      ] NoContent) :<|>
  Delete '[JSON] NoContent :<|>
  Head '[JSON] NoContent
  )

type Digests = Capture "digest" Digest :> (
  Head '[JSON] (Headers '[
    Header "Content-Length" Int,
    Header "Docker-Content-Digest" Digest
    ] NoContent) :<|>
  Get '[JSON] NoContent :<|>
  Delete '[JSON] NoContent
  )

type Upload = "uploads" :> (
  PostAccepted '[JSON] (Headers '[
    Header "Location" URI,
    Header "Range" String,
    Header "Docker-Upload-UUID" UUID
  ] NoContent) :<|>
  Capture "uuid" UUID :> (
    Get '[JSON] NoContent :<|>
    ReqBody '[OctetStream] ByteString :>
      Header "range" String :>
      PatchNoContent '[JSON] (Headers '[
        Header "Location" URI,
        Header "Range" String,
        Header "Docker-Upload-UUID" UUID
      ] NoContent) :<|>
    QueryParam "digest" Digest :>
      Put '[JSON] NoContent :<|>
    Delete '[JSON] NoContent
    )
  )

instance FromHttpApiData UUID where
  parseUrlPiece text = case (fromText text) of
    Nothing -> Left $ T.append "Invalid UUID" text
    Just uuid -> Right uuid
instance ToByteString URI where
  builder = lazyByteString . pack . show
instance ToHttpApiData UUID where
  toUrlPiece = toText
  toHeader = toASCIIBytes

instance ToByteString UUID where
  builder = lazyByteString . toLazyASCIIBytes
