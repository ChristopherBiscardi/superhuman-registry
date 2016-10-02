{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where


import           Control.Monad.Except
import           Control.Monad.Reader                 (runReaderT)
import           Data.Monoid                          as M
import           Data.Text                            (Text)
import           Hasql.Connection                     (settings)
import qualified Hasql.Pool                           as P
import           Katip.Core
import           Katip.Monadic
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Prometheus    (PrometheusSettings (..),
                                                       prometheus)
import           Network.Wai.Middleware.RequestLogger
import           Prometheus                           (register)
import           Prometheus.Metric.GHC                (ghcMetrics)
import           Servant

import           Config
import           Env
import           Initialization
import           Logging
import           SR.Blobs                             (blobServer)
import           SR.Health
import           SR.Metadata                          (metadataServer)
import           SR.Routes
import           SR.Types

-- | Initialize the app and setup Servant Type-related boilerplate
startApp :: IO ()
startApp = do
  register ghcMetrics
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True
  pool <- pgSettings
  logEnv <- initLogging
  print "booting"
  run 8080 $ logStdoutDev
           $ promMiddleware
           $ app
           $ AppConfig pool M.mempty mempty logEnv

readerServer :: AppConfig -> Server API
readerServer cfg = enter (convertApp cfg) apiServer

convertApp :: AppConfig -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

app :: AppConfig -> Application
app cfg = serve api $ readerServer cfg

-- | Organize Handlers
apiServer :: ServerT API App
apiServer = v2
       :<|> (
              metadataServer
         :<|> getCatalog
            )
       :<|> checkHealth

getCatalog :: App NoContent
getCatalog = undefined

v2 :: App (Headers '[Header "Docker-Distribution-API-Version" String] NoContent)
v2 = do
  $(logTM) InfoS "registry/2.0"
  return $ addHeader "registry/2.0" NoContent

