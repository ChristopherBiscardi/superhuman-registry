module SR.Blobs where

import Servant

import SR.Routes
import Config (App)
import SR.Types

blobServer :: Name -> ServerT Blobs App
blobServer name' = digests
        :<|> uploadBlob
        :<|> withUUID
       where digests digest' = digestsServer name' digest'
             withUUID uuid' = uuidBlob name' uuid'
                         :<|> uuidBlob name' uuid'
                         :<|> uuidBlob name' uuid'
                         :<|> uuidBlob name' uuid'

uuidBlob :: Name -> UUID -> App NoContent
uuidBlob = undefined

blobTODO :: Digest -> App NoContent
blobTODO name = undefined

uploadBlob :: App NoContent
uploadBlob = undefined

digestsServer :: Name -> ServerT Digests App
digestsServer name' digest' = blobTODO digest'
                   :<|> blobTODO digest'
                   :<|> blobTODO digest'
