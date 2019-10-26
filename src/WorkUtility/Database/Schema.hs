{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
module WorkUtility.Database.Schema where

import qualified Database.Persist.TH           as Tmpl
import           Database.Persist.Sql
import           Data.Time.Clock
import           Data.Text
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           WorkUtility.Types              ( HasTimestamp
                                                , createdAt
                                                , updatedAt
                                                , Config
                                                , HasDb
                                                , getDb
                                                )

Tmpl.share
  [ Tmpl.mkPersist Tmpl.sqlSettings { Tmpl.mpsGenerateLenses = True }
  , Tmpl.mkMigrate "migrateAll"
  ] [Tmpl.persistLowerCase|
Project sql=projects
  projectId String
  title String
  description String Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Task sql=todos
  title String
  description String Maybe
  status String
  projectId ProjectId
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show
|]

instance HasTimestamp Project where
  createdAt = projectCreatedAt
  updatedAt = projectUpdatedAt
instance HasTimestamp Task where
  createdAt = taskCreatedAt
  updatedAt = taskUpdatedAt

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = lift ask >>= getDb >>= liftIO . runSqlPool query
