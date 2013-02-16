-- |
-- Module      : Ligature.Splices
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.Splices (
      navSplices
    , dashSplices
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad
import Data.Char           (toUpper)
import Data.Text           (Text)
import Heist
import Heist.Interpreted
import Ligature.Types
import Ligature.URL

import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Text.XmlHtml        as X

navSplices :: Monad m => HashMap Dash -> [(Text, Splice m)]
navSplices hmap = [("nav", mapSplices (uncurry navSplice) $ H.toList hmap)]

navSplice :: Monad m => Key -> Dash -> Splice m
navSplice (Key k) d = runChildrenWithText
    [ ("navLink", "/dashboards/" `T.append` k)
    , ("navName", dashName d)
    , ("navAlt",  dashDesc d)
    ]

dashSplices :: Monad m => Dash -> [(Text, Splice m)]
dashSplices d =
    [ ("dashName", textSplice $ dashName d)
    , ("dashDesc", textSplice $ dashDesc d)
    , ("graphs",   graphSplices d)
    ]

graphSplices :: Monad m => Dash -> Splice m
graphSplices d = do
    ps <- (map parseParam . X.elementAttrs) `liftM` getParamNode
    flip mapSplices indexes $ \(i, (k, g)) -> runChildrenWith $
        [ ("graphNum",    textSplice . T.pack $ show i)
        , ("graphActive", textSplice $ if i == 0 then "active" else "")
        , ("graphName",   textSplice $ graphName g)
        , ("graphDesc",   textSplice $ graphDesc g)
        , ("graphUrl",    textSplice $ graphUrl d k ps)
        ] ++ map f ps
  where
    f = (\(k, v) -> (k, textSplice v)) . toFragment
    indexes  = zip [0..] . H.toList $ dashGraphs d

graphUrl :: Dash -> Key -> [Param] -> Text
graphUrl d (Key g) ps = T.concat
    [ "/dashboards/"
    , keyText . textKey $ dashName d -- ^ FIXME
    , "/graphs/"
    , g
    , "?"
    , fromFragments $ pack ps
    ]
