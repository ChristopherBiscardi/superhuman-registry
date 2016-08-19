module SR.Metadata where

import Servant

import SR.Routes
import Config (App)
import SR.Types
import SR.Blobs (blobServer)

metadataServer :: ServerT Metadata App
metadataServer name' = getTags name'
                  :<|> manifestsServer name'
                  :<|> blobServer name'

manifestsServer :: Name -> ServerT Manifests App
manifestsServer name' ref' = manifestTODO name' ref'
  :<|> manifestTODO name' ref'
  :<|> manifestTODO name' ref'
  :<|> manifestTODO name' ref'


manifestTODO :: Name -> Ref -> App NoContent
manifestTODO name _ = undefined

getTags :: Name -> App NoContent
getTags name' = undefined
