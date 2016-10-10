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
import qualified Hasql.Pool                     as P
import           Hasql.Query                    (Query, statement)
import           Hasql.Session                  (query)
import qualified Hasql.Transaction              as HT

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
  maybeSize <- runPG (query (T.pack $ show digest) doesBlobExist)
  case maybeSize of
    Just size -> return BLOB_EXISTS
    Nothing -> return UNKNOWN_BLOB
  where
    doesBlobExist :: Query (Text) (Maybe Int64)
    doesBlobExist =
      statement sql encoder decoder True
      where
        sql =
          "SELECT size FROM sr.blobs WHERE id = $1"
        encoder =
          E.value E.text
        decoder =
          D.maybeRow $ D.value D.int8
