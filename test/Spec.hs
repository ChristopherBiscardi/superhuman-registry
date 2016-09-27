{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Control.Exception (evaluate)
import SR.Types
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString (ByteString)
import Data.Aeson
import qualified Crypto.Hash as CH
import qualified Data.ByteString.Lazy.Char8 as C (readFile)

parseManifest :: L.ByteString -> Either String Manifest
parseManifest = eitherDecode'

parsedManifest = V2_2 {
  schemaVersion = 2,
  mediaType = Manifest_V2_JSON,
  config = Config {
      cMediaType = V1_JSON,
      cSize = Just 7023,
      cDigest = "sha256:b5b2b2c507a0944348e0303114d8d93aaaa081732b86451d9bce1f432a537bc7"
      },
  layers = [
      Layer {
          lMediaType = Diff,
          lSize = Just 32654,
          lDigest = "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
          lUrls = Nothing
          },
      Layer {
          lMediaType = Diff,
          lSize = Just 16724,
          lDigest = "sha256:3c3a4604a545cdc127456d94e421cd355bca5b528f4a9c1905b15da2eb4a4c6b",
          lUrls = Nothing
          },
      Layer {
          lMediaType = Diff,
          lSize = Just 73109,
          lDigest = "sha256:ec4b8955958665577945c89419d1af06b5f7636b4ac3da7f12184802ad867736",
          lUrls = Nothing
          }
      ]
  }

main :: IO ()
main = hspec $ do
  describe "Manifest" $ do
    it "parses the example Manifest v2" $ do
--      let Just digestA = CH.digestFromByteString ("sha256:b5b2b2c507a0944348e0303114d8d93aaaa081732b86451d9bce1f432a537bc7" :: ByteString)
      json <- C.readFile "./test/manifest-v2.json"
      case parseManifest json of
        Left err -> fail err
        Right m -> m `shouldBe` parsedManifest
      -- head [23 ..] `shouldBe` (23 :: Int)
