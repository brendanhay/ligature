{-# LANGUAGE TemplateHaskell, CPP #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
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
import Control.Exception             (SomeException, try)
import Control.Lens
import Control.Monad                 (liftM)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Data.ByteString               (ByteString)
import Data.Char                     (toUpper)
import Ligature.Config.CLI
import Ligature.Config.JSON
import Ligature.Splices
import Ligature.Types
import Ligature.URL
import Network.URI                   (URI)
import Snap.Core
import Snap.Http.Server       hiding (Config)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

class RequiredParam a where
    requireParam :: MonadSnap m => ByteString -> m a

instance RequiredParam Bool where
    requireParam = liftM (read . f) . requireParam
      where
        f s = (toUpper $ head s) : tail s

instance RequiredParam Int where
    requireParam = liftM read . requireParam

instance RequiredParam Key where
    requireParam = liftM byteKey . requireParam

instance RequiredParam String where
    requireParam = liftM BS.unpack . requireParam

instance RequiredParam ByteString where
    requireParam name = do
        val <- getParam name
        maybe (error $ "Missing param " ++ BS.unpack name)
              return val

instance RequiredParam Param where
    requireParam name = do
        val <- E.decodeUtf8 <$> requireParam name
        return $ parseParam (E.decodeUtf8 name, val)

allParamsExcept :: (MonadSnap m, RequiredParam a) => [BS.ByteString] -> m [a]
allParamsExcept es = do
    ps <- M.toList <$> getParams
    mapM (requireParam . fst) $ filter (not . flip elem es . fst) ps

main :: IO ()
main = do
    cfg <- snapConfig
    (ms, app, cleanup) <- runSnaplet Nothing . site $ appConfig cfg
    hPutStrLn stderr $ T.unpack ms
    print cfg
    print $ appConfig cfg
    _   <- try $ httpServe cfg app :: IO (Either SomeException ())
    cleanup

site :: Config -> SnapletInit App App
site cfg = makeSnaplet "ligature" "Graphite Dashboards" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    m <- liftIO . parse $ dashboards cfg
    addRoutes  $ routes (graphite cfg) m
    addSplices $ navSplices m
    return $ App h

routes :: HasHeist a => URI -> HashMap Dash -> [(ByteString, Handler a b ())]
routes uri hmap =
    [ ("/dashboards/:dashboard/graphs/:graph", graph uri hmap)
    , ("/dashboards/:dashboard",               dashboard hmap)
    , ("",                                     serveDirectory "public")
    ]

graph :: HasHeist a => URI -> HashMap Dash -> Handler a b ()
graph uri hmap = do
    d  <- requireParam "dashboard"
    g  <- requireParam "graph"
    ps <- allParamsExcept ["dashboard", "graph"]
    liftIO (graphData uri (findGraph hmap d g) ps) >>= writeBS

dashboard :: HasHeist a => HashMap Dash -> Handler a b ()
dashboard hmap = do
    d <- requireParam "dashboard"
    renderWithSplices "dashboard" . dashSplices $ findDash hmap d
