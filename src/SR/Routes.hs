{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SR.Routes where

import Data.Text (Text)
import Data.Text.IO
import Servant

import SR.Types

api :: Proxy API
api = Proxy

type Head = Verb 'HEAD 200

type V2Base = "v2" :> Get '[JSON] (Headers '[
  Header "Docker-Distribution-API-Version" String
  ] NoContent)

-- | Main API Type
type API = V2Base :<|> "v2" :> V2API

-- | V2 API Definition
type V2API = Metadata :<|>
  "blobs" :> Blobs
  :<|> "_catalog" :> Get '[JSON] NoContent

type Tags = "tags" :> "list" :> Get '[JSON] NoContent 

type Metadata = Capture "name" Name :> (
  Tags :<|> Manifests
  )

type Blobs = Digests :<|> Upload

type Manifests = "manifests" :> Capture "reference" Ref :> (
  Get '[JSON] NoContent :<|>
  Put '[JSON] NoContent :<|>
  Delete '[JSON] NoContent :<|>
  Head '[JSON] NoContent
  ) 

type Digests = Capture "digest" Digest :> (
  Head '[JSON] NoContent :<|>
  Get '[JSON] NoContent :<|>
  Delete '[JSON] NoContent
  )

type Upload = "uploads" :> (
  Post '[JSON] NoContent :<|>
  Capture "uuid" UUID :> (
    Get '[JSON] NoContent :<|>
    Patch '[JSON] NoContent :<|>
    Put '[JSON] NoContent :<|>
    Delete '[JSON] NoContent
    )
  )

