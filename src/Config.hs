{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import           Control.Monad.Except       (ExceptT, MonadError, MonadIO)
import           Control.Monad.Reader       (ReaderT, asks)
import           Control.Monad.Reader.Class (MonadReader)
--import           Database.PostgreSQL.Simple (Connection)
import qualified Hasql.Pool                 as P (Pool)
import           Katip
import           Servant                    (ServantErr)

data AppConfig = AppConfig { acPGPool     :: P.Pool
                           , acKNamespace :: Namespace
                           , acKContext   :: LogContexts
                           , acLogEnv     :: LogEnv
                           }

newtype App a = App { runApp :: ReaderT AppConfig (ExceptT ServantErr IO) a
                    } deriving ( Functor, Applicative, Monad, MonadReader AppConfig, MonadError ServantErr, MonadIO)

instance Katip App where
  getLogEnv = asks acLogEnv


instance KatipContext App where
  getKatipContext = asks acKContext
  getKatipNamespace = asks acKNamespace
