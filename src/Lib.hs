{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( startApp
    ) where


import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Data.Monoid as M
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Prometheus (PrometheusSettings (..),
                                                    prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Katip.Core
import Katip.Monadic
import Servant
import Network.Wai.Middleware.RequestLogger

import Config
import Initialization
import Logging
import SR.Blobs (blobServer)
import SR.Metadata (metadataServer)
import SR.Routes
import SR.Types

-- | Initialize the app and setup Servant Type-related boilerplate
startApp :: IO ()
startApp = do
  register ghcMetrics
  pgPool <- initPG
--  withResource pgPool initUserBackend
  logEnv <- initLogging
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True
  print "booting"
  run 8080 $ logStdoutDev $ promMiddleware $ app $ AppConfig pgPool M.mempty mempty logEnv

readerServer :: AppConfig -> Server API
readerServer cfg = enter (convertApp cfg) apiServer

convertApp :: AppConfig -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

app :: AppConfig -> Application
app cfg = serve api $ readerServer cfg

-- | Organize Handlers
apiServer :: ServerT API App
apiServer = v2
       :<|> metadataServer
       :<|> getCatalog

getCatalog :: App NoContent
getCatalog = undefined

v2 :: App (Headers '[Header "Docker-Distribution-API-Version" String] NoContent)
v2 = do
  $(logTM) InfoS "registry/2.0"
  return $ addHeader "registry/2.0" NoContent
