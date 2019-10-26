{-# LANGUAGE OverloadedStrings #-}
module WorkUtility.Server where

import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Types.Status      ( status404 )
import           System.Environment
import           Web.Scotty
import           Web.PathPieces

import           WorkUtility.Types              ( ScottyD )

server :: ScottyD ()