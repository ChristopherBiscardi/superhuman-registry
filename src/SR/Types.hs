{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SR.Types where

import Data.Text (Text)
import Servant

-- | Type Utilities
newtype Name = Name Text deriving (Show, FromHttpApiData)
newtype Ref = Ref Text deriving (Show, FromHttpApiData)
newtype Digest = Digest Text deriving (Show, FromHttpApiData)
newtype UUID = UUID Text deriving (Show, FromHttpApiData)


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
