{-# LANGUAGE OverloadedStrings #-}
module Backend where

import           Contravariant.Extras.Contrazip (contrazip2, contrazip3)
import           Control.Monad.Reader
import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.UUID                      (UUID (..))
import           GHC.Int                        (Int32, Int64)
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.LO
import           Hasql.LO.Types
import qualified Hasql.Pool                     as P
import           Hasql.Query                    (Query, statement)
import           Hasql.Session                  (query)
import qualified Hasql.Transaction              as HT
import           Servant.Server
import Servant

import           Backend.Types                  (BlobExistance (..))
import           Config
import           SR.Types
import           Utils                          (runPG)

type Reponame = Text

-- | Insert a new upload for a Repos
startNewUpload :: Reponame -> App UUID
startNewUpload repo = runPG (query repo insertNewUpload)

-- | PG automatically creates uuid, created_at and modified_at
insertNewUpload :: Query Text UUID
insertNewUpload =
  statement sql encoder decoder True
  where
    sql =
      "INSERT INTO sr.blob_uploads (repo_name) VALUES ($1) RETURNING id"
    encoder =
      E.value E.text
    decoder =
      D.singleRow (D.value D.uuid)

receivePushContent :: Maybe Range -> ByteString -> UUID -> Text -> App ()
receivePushContent range' blob uuid reponame = do
  let r' = case range' of
         Nothing -> Range 0 0
         Just r -> r
  _ <- runPG $ HT.run (appendData r' blob uuid reponame) HT.ReadCommitted HT.Write
  return ()

appendData :: Range -> ByteString -> UUID -> Text -> HT.Transaction ()
appendData (Range offset end') blob uuid reponame = do
  loID <- HT.query (uuid, reponame) getLOID
  case loID of
    Nothing -> do
      oid <- createFromByteString Nothing blob
      _ <- HT.query (oid, uuid, reponame) insertLOID
      return ()
    Just oid -> do
      _ <- put oid (fromIntegral offset) blob
      return ()

insertLOID :: Query (Int32, UUID, Text) ()
insertLOID =
  statement sql encoder decoder True
  where
    sql =
      "UPDATE sr.blob_uploads SET lo_id = $1 WHERE id = $2 AND repo_name = $3;"
    encoder =
      contrazip3 (E.value E.int4)
                 (E.value E.uuid)
                 (E.value E.text)
    decoder = D.unit

getLOID :: Query (UUID, Text) (Maybe Int32)
getLOID =
  statement sql encoder decoder True
  where
    sql =
      "SELECT (lo_id) FROM sr.blob_uploads WHERE id = $1 AND repo_name = $2"
    encoder =
      contrazip2 (E.value E.uuid)
                 (E.value E.text)
    decoder =
      D.singleRow (D.nullableValue D.int4)

-- | HEAD blob
-- TODO: fix string handling
-- TODO: Change Digest to `Digest SHA256`
headBlob :: Digest -> App BlobExistance
headBlob (Digest digest) = do
  maybeSize <- runPG (query (T.pack $ show digest) fetchSizeIfExists)
  case maybeSize of
    Just size -> return BLOB_EXISTS
    Nothing -> return UNKNOWN_BLOB
  where
    fetchSizeIfExists :: Query (Text) (Maybe Int32)
    fetchSizeIfExists =
      statement sql encoder decoder True
      where
        sql =
          "SELECT blob_size FROM sr.blobs WHERE blob_digest = $1"
        encoder =
          E.value E.text
        decoder =
          D.maybeRow $ D.value D.int4

-- | confirm blob upload
-- 1. Ensure blob exists in blob_uploads table
-- 2. sha256 hash using pgcrypto
-- 3. seek to end of blob and tell size
-- 4. insert into repo_blobs table with size, lo_id and digest hash
registerBlob :: Reponame -> UUID -> Digest -> App (Int32)
registerBlob reponame uploadID (Digest digest) = do
  let digest' = T.pack $ show digest
  maybeSize <- runPG $ HT.run (solidifyBlob uploadID reponame digest') HT.ReadCommitted HT.Write
  case maybeSize of
    Nothing -> throwError err500 { errBody = "No Blob exists for upload" }
    Just size -> return size
  where
    solidifyBlob :: UUID -> Reponame -> Text -> HT.Transaction (Maybe Int32)
    solidifyBlob uuid reponame digest'' = do
      loID <- HT.query (uploadID, reponame) getLOID
      case loID of
        Nothing -> return Nothing
        Just oid -> do
          fd <- open oid READ
          sha <- hashBlob fd
          size <- getBlobSize fd
          -- | insert blob into sr.blobs
          _ <- HT.query (sha, oid, size) insertBlob
          -- | associate blob with repo by inserting into sr.repo_blobs
          _ <- HT.query (sha, reponame) associateBlob
          return $ Just size
    -- | hashBlob uses pgcrypto to sha256 a blob
    hashBlob :: FD -> HT.Transaction Text
    hashBlob t = undefined
    -- | seek to end and read length of blob
    getBlobSize :: FD -> HT.Transaction Int32
    getBlobSize fd = do
      _ <- seek fd 0 END
      size <- tell fd
      return size
    -- | TODO: make sure digest is `sha256:text` form
    insertBlob :: Query (Text, OID, Int32) ()
    insertBlob =
      statement sql encoder decoder True
      where
        sql =
          "INSERT INTO sr.blobs (blob_digest, lo_id, blob_size) VALUES ($1, $2, $3)"
        encoder =
          contrazip3 (E.value E.text)
                     (E.value E.int4)
                     (E.value E.int4)
        decoder = D.unit
    associateBlob :: Query (Text, Text) ()
    associateBlob =
      statement sql encoder decoder True
      where
        sql =
          "INSERT INTO sr.repo_blobs (blob_digest, repo_name) VALUES ($1, $2)"
        encoder =
          contrazip2 (E.value E.text)
                     (E.value E.text)
        decoder = D.unit
