-- |
-- Module      : Ligature.Splices
-- Copyright   : (c) 2012 Brendan Hay <brendan.g.hay@gmail.com>
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

import Data.Monoid       (Monoid)
import Data.Text         (Text)
import Heist
import Heist.Interpreted
import Ligature.Types

import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Text.XmlHtml        as X

navSplices :: Monad m => HashMap Dash -> [(Text, Splice m)]
navSplices hmap = [("nav", mapSplices (uncurry navSplice) $ H.toList hmap)]

navSplice :: Monad m => Text -> Dash -> Splice m
navSplice k d = runChildrenWithText
    [ ("navLink", "/dashboards/" `T.append` k)
    , ("navName", dashName d)
    , ("navAlt",  dashDesc d)
    ]

dashSplices :: Monad m => Dash -> [(Text, Splice m)]
dashSplices d =
    [ ("dashName", textSplice $ dashName d)
    , ("dashDesc", textSplice $ dashDesc d)
    , ("graphs", graphSplices $ dashGraphs d)
    ]

graphSplices :: Monad m => HashMap Graph -> Splice m
graphSplices gs = do
    n  <- getParamNode
    let x = getAttribute 300 "width"  n
        y = getAttribute 200 "height" n
    indexedSplices (H.toList gs) $ \(i, (k, g)) -> runChildrenWith
        [ ("graphNum",    textSplice $ num i)
        , ("graphActive", textSplice $ if i == 0 then "active" else "")
        , ("graphName",   textSplice $ graphName g)
        , ("graphDesc",   textSplice $ graphDesc g)
        , ("graphWidth",  textSplice $ num x)
        , ("graphHeight", textSplice $ num y)
        , ("graphUrl",    textSplice . T.toLower $
             "/dashboards/nginx/graphs/" `T.append` T.concat [k, "?width=", T.pack $ show x, "&height=", T.pack $ show y])
        ]
  where
    num :: Integer -> Text
    num = T.pack . show

indexedSplices :: (Monad m, Enum a, Num a, Monoid c)
               => [b]
               -> ((a, b) -> m c)
               -> m c
indexedSplices ss f = mapSplices f $ zip [0..] ss

getAttribute :: Read a => a -> Text -> X.Node -> a
getAttribute def name = maybe def (read . T.unpack) . X.getAttribute name
