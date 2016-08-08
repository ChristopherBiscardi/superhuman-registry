module SR.Blobs where

import Servant

import SR.Routes
import Config (App)
import SR.Types

blobServer :: ServerT Blobs App
blobServer = digests
        :<|> uploadBlob
        :<|> withUUID
       where digests digest' = digestsServer digest'
             withUUID uuid' = uuidBlob uuid' :<|> uuidBlob uuid' :<|> uuidBlob uuid' :<|> uuidBlob uuid'

uuidBlob :: UUID -> App NoContent
uuidBlob = undefined

blobTODO :: Digest -> App NoContent
blobTODO name = undefined

uploadBlob :: App NoContent
uploadBlob = undefined

digestsServer :: ServerT Digests App
digestsServer digest' = blobTODO digest'
                   :<|> blobTODO digest'
                   :<|> blobTODO digest'
