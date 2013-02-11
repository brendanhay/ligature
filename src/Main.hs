{-# LANGUAGE TemplateHaskell, CPP #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
      main
    ) where

import Control.Applicative
import Control.Exception      (SomeException, try)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Ligature.Config.JSON
import Ligature.Handlers
import Ligature.Splices
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Config
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.IO

import qualified Data.Text as T

#ifdef DEVELOPMENT
import Snap.Loader.Dynamic
#else
import Snap.Loader.Static
#endif

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

main :: IO ()
main = do
    (cfg, site, free) <- $(loadSnapTH [| getConf |] 'getActions ["snaplets/heist/templates"])
    _ <- try $ httpServe cfg site :: IO (Either SomeException ())
    free

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions cfg = do
    (msgs, site, free) <- runSnaplet (appEnvironment =<< getOther cfg) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, free)

app :: SnapletInit App App
app = makeSnaplet "ligature" "Graphite Dashs" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    m <- liftIO $ parse "dashboards"
    addRoutes  $ routes m
    addSplices $ navSplices m
    wrapSite (<|> serveDirectory "public")
    return $ App h
