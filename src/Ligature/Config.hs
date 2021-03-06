{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Ligature.Config
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.Config (
      SnapConfig
    , Config(..)
    , snapConfig
    , appConfig
    ) where

import Data.Monoid
import Data.Maybe
import Data.Data
import Network.URI
import Snap.Core
import Snap.Http.Server      hiding (Config)
import System.Console.GetOpt

import qualified Snap.Http.Server as S

type SnapConfig = S.Config Snap Config

data Config = Config
    { dashboardDir :: FilePath
    , graphiteUrl  :: URI
    } deriving (Typeable)

instance Show Config where
    show Config{..} = unlines
        [ "Ligature:"
        , "dashboard directory: " ++ dashboardDir
        , "graphite url: "        ++ show graphiteUrl
        ]

instance Monoid Config where
    mempty      = Config "dashboards" (fromJust $ parseURI "http://graphite")
    mappend a b = a
        { dashboardDir = dashboardDir b
        , graphiteUrl  = graphiteUrl b
        }

appConfig :: SnapConfig -> Config
appConfig = fromJust . getOther

snapConfig :: IO SnapConfig
snapConfig = extendedCommandLineConfig
    (options (fromMaybe mempty $ getOther def) ++ optDescrs def)
    mappend def
  where
    def = foldl (flip (.)) id
        [ setPort 8080
        , setAccessLog $ fileLog "access.log"
        , setErrorLog  $ fileLog "error.log"
        ] $ emptyConfig

fileLog :: FilePath -> ConfigLog
fileLog = ConfigFileLog . ("/var/log/ligature/" ++)

options :: Config -> [OptDescr (Maybe SnapConfig)]
options cfg = map (fmapOpt $ fmap (`setOther` mempty))
    [ graphiteOption cfg
    , dashboardsOption cfg
    ]

graphiteOption :: Config -> OptDescr (Maybe Config)
graphiteOption cfg = option "graphite"
    (\s -> cfg { graphiteUrl = fromJust $ parseURI s }) "URL" $
    "graphite url, default " ++ show (graphiteUrl cfg)

dashboardsOption :: Config -> OptDescr (Maybe Config)
dashboardsOption cfg = option "dashboards"
    (\s -> cfg { dashboardDir = s }) "URL" $
    "dashboards directory, default " ++ show (dashboardDir cfg)

option :: String -> (String -> a) -> String -> String -> OptDescr (Maybe a)
option flag upd typ help = Option [] [flag] (ReqArg (Just . upd) typ) help
