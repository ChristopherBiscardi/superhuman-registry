module SR.Metadata where

import Servant

import SR.Routes
import Config (App)
import SR.Types
import SR.Blobs (blobServer)

metadataServer :: ServerT Metadata App
metadataServer namespace' name' = getTags namespace' name'
                  :<|> manifestsServer namespace' name'
                  :<|> blobServer namespace' name'

manifestsServer :: Namespace -> Name -> ServerT Manifests App
manifestsServer namespace' name' ref' = manifestTODO namespace' name' ref'
  :<|> manifestTODO namespace' name' ref'
  :<|> manifestTODO namespace' name' ref'
  :<|> manifestTODO namespace' name' ref'


manifestTODO :: Namespace -> Name -> Ref -> App NoContent
manifestTODO namespace' name _ = undefined

getTags :: Namespace -> Name -> App NoContent
getTags namespace' name' = undefined
