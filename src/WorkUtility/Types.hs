{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
module WorkUtility.Types where

import           Data.Aeson
import           GHC.Generics
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Serialize
import           Data.Text
import           Data.Time.Clock
import           Control.Lens            hiding ( (.=) )
import           Database.Persist.Sql
import           Database.Persist.TH
import           Web.Scotty.Trans
import           Web.Scotty

data EnvironmentMode = Test | Devel | Production deriving (Eq, Show)

data Config = Config
  { envMode :: EnvironmentMode
  , db :: SqlBackend
  , logger :: !(String -> IO ())
  }

class HasEnvironmentMode a where
  getEnvMode :: a -> EnvironmentMode
instance HasEnvironmentMode EnvironmentMode where
  getEnvMode = id
instance HasEnvironmentMode Config where
  getEnvMode = envMode

class HasDb a where
  getDb :: a -> ConnectionPool
instance HasDb ConnectionPool where
  getDb = id
instance HasDb Config where
  getDb = db

class HasLogger a where
  getLogger :: a -> (String -> IO ())
instance HasLogger (String -> IO ()) where
  getLogger = id
instance HasLogger Config where
  getLogger = logger

-- class SuccessT s where
--   getSuccess :: s -> ActionM ()

-- class ErrorT e where
--   getError :: e -> ActionM ()

type ScottyD = ScottyT Text (ReaderT Config IO)
type ActionD = ActionT Text (ReaderT Config IO)

class HasTimestamp s where
  createdAt :: Lens' s UTCTime
  updatedAt :: Lens' s UTCTime
