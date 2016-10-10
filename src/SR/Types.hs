{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module SR.Types where

import qualified Crypto.Hash                   as CH
import           Data.Aeson
import           Data.Aeson.Types              (Options (..), defaultOptions)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString as B (intercalate)
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Builder       (byteString, stringUtf8)
import           Data.ByteString.Conversion.To (ToByteString (..))
import           Data.Char                     (toLower)
import           Data.Text                     (Text)
import qualified Data.Text                     as T (unpack)
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Text.Encoding            as TE
import           GHC.Generics
import           Network.URI                   (parseURI)
import           Servant
import           Text.Parser.Combinators       (sepBy1)
import           Text.Trifecta
import           Text.Trifecta.Delta           (Delta (..))

-- | TODO: replace `String` with `CDigest` to validate digests and
-- reject manifests with invalid digests
newtype CDigest = CDigest (CH.Digest CH.SHA256) deriving (Show, Eq)
instance FromJSON CDigest where
  parseJSON = withText "SHA256 Digest" $ \txt -> do
    case CH.digestFromByteString $ encodeUtf8 txt of
        Nothing -> do
          fail $ show txt ++ " is not a digest"
        Just v -> pure $ CDigest v
instance ToByteString CDigest where
  builder (CDigest hash) = stringUtf8 $ "sha256:" ++ show hash

instance FromJSON URI where
  parseJSON = withText "URI" $ \txt -> case parseURI $ T.unpack txt of
    Nothing -> fail "not a URI"
    Just v -> pure v
-- | Type Utilities
newtype Name = Name Text deriving (Show, FromHttpApiData, ToHttpApiData)
newtype Namespace = Namespace Text deriving (Show, FromHttpApiData, ToHttpApiData)
newtype Ref = Ref Text deriving (Show, FromHttpApiData)
newtype Digest = Digest ByteString deriving (Show)

data Range = Range { start :: Int
                   , end   :: Int
                   } deriving Show
parseRange :: Parser Range
parseRange = do
  start' <- some digit
  _ <- char '-'
  end' <- some digit
  return $ Range (read start') (read end')

instance FromHttpApiData Range where
  parseUrlPiece text = case (parseString parseRange (Columns 0 0) $ show text) of
    Failure err -> undefined
    Text.Trifecta.Success range -> Right range
instance ToByteString Range where
  builder (Range start' end') = byteString $ B.intercalate "-" [C.pack $ show start', C.pack $ show end']

instance FromHttpApiData Digest where
  parseUrlPiece text = Right $ Digest $ TE.encodeUtf8 text
instance ToByteString Digest where
  builder (Digest bs) = byteString bs

-- | application/vnd.docker.distribution.manifest.v2+json: New image manifest format (schemaVersion = 2)
--   application/vnd.docker.distribution.manifest.list.v2+json: Manifest list, aka "fat manifest"
data Manifest = V2_2 { schemaVersion :: Int
                     , mediaType     :: Manifest_V2_JSON
                     , config        :: Config
                     , layers        :: [Layer]
                     } deriving (Generic, Show, Eq)
--              | V2_2_LIST
instance FromJSON Manifest

data V1_JSON = V1_JSON deriving (Generic, Show, Eq)
instance FromJSON V1_JSON where
  parseJSON = withText "Config Media Type V1 JSON" $
    \mediaType -> case mediaType of
      "application/vnd.docker.container.image.v1+json" -> pure V1_JSON
      otherwise -> fail "incorrect config mediatype string"

data Config = Config { cMediaType :: V1_JSON
                     -- | Sometimes the 1.12 docker client doesn't send the size
                     , cSize      :: Maybe Int
                     , cDigest    :: String -- CDigest
                     } deriving (Generic, Show, Eq)
instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerCaseFirstChar . drop 1 }

data LayerMediaTypes = Diff | Foreign
                     deriving (Generic, Show, Eq)
instance FromJSON LayerMediaTypes where
  parseJSON = withText "Layer Media Types" $
    \mediaType -> case mediaType of
      "application/vnd.docker.image.rootfs.diff.tar.gzip" -> pure Diff
      "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip" -> pure Foreign
      otherwise -> fail "no matching layer mediatype"

data Layer = Layer { lMediaType :: LayerMediaTypes
                     -- | Sometimes the 1.12 docker client doesn't send the size
                   , lSize      :: Maybe Int
                   , lDigest    :: String -- CDigest
                   , lUrls      :: Maybe [URI]
                   } deriving (Generic, Show, Eq)
instance FromJSON Layer where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerCaseFirstChar . drop 1 }

data Manifest_V2_JSON = Manifest_V2_JSON deriving (Generic, Show, Eq)
instance FromJSON Manifest_V2_JSON where
  parseJSON = withText "Manifest V2 JSON" $
    \mediaType -> case mediaType of
      "application/vnd.docker.distribution.manifest.v2+json" -> pure Manifest_V2_JSON
      otherwise -> fail "incorrect manifest version string"

data ERRORS = BLOB_UNKNOWN
            | BLOB_UPLOAD_INVALID
            | BLOB_UPLOAD_UNKNOWN
            | DIGEST_INVALID
            | MANIFEST_BLOB_UNKNOWN
            | MANIFEST_INVALID
            | MANIFEST_UNKNOWN
            | MANIFEST_UNVERIFIED
            | NAME_INVALID
            | NAME_UNKNOWN
            | SIZE_INVALID
            | TAG_INVALID
            | UNAUTHORIZED
            | DENIED
            | UNSUPPORTED

lowerCaseFirstChar :: String -> String
lowerCaseFirstChar (x:xs) = [toLower x] ++ xs
