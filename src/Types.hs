{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Data.Aeson
import           Data.HashMap.Strict
import           Data.Text           (Text)

data JSONFieldErrors = JSONFieldErrors { errors :: HashMap Text Text }

instance ToJSON (JSONFieldErrors) where
  toJSON (JSONFieldErrors hash) = object [ "errors" .= toJSON hash ]
