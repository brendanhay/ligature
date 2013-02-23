{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.IO.Class
import Control.Monad
import Data.Char           (toUpper)
import Data.Maybe
import Data.String
import Data.Text           (Text)
import Heist
import Heist.Interpreted
import Heist.Splices
import Snap.Core
import Ligature.Types
import Ligature.URL

import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as E
import qualified Text.XmlHtml        as X

navSplices :: (MonadSnap m, MonadSnap (HeistT m m))
           => HashMap Dash
           -> [(Text, Splice m)]
navSplices hmap =
    [ ("nav",         mapSplices (uncurry navSplice) $ H.toList hmap)
    , ("ifDashboard", ifDashboardSplice)
    , ("currentUrl",  currentUrl >>= \u -> return [X.TextNode u])
    ]

ifDashboardSplice :: (Monad m, MonadSnap (HeistT m m)) => Splice m
ifDashboardSplice = do
    uri <- currentUrl
    if "dashboards" `elem` T.splitOn "/" uri
       then getParamNode >>= return . X.childNodes
       else return []

menuEntrySplice :: MonadSnap m => Splice m
menuEntrySplice = do
    requestPath <- lift $ withRequest (return . rqURI)
    node <- getParamNode
    let setActive n = if X.getAttribute "href" node == Just (E.decodeUtf8 requestPath)
                       then X.setAttribute "class" "active" n
                       else n
    let aNode  = X.Element "a" [("href", fromMaybe "/" $ X.getAttribute "href" node)] $ [X.TextNode (X.nodeText node)]
    return [setActive $ X.Element "li" [] [aNode]]

currentUrl :: MonadSnap m => m Text
currentUrl = withRequest (return . head . T.splitOn "?" . E.decodeUtf8 . rqURI)

navSplice :: Monad m => Key -> Dash -> Splice m
navSplice (Key k) d = runChildrenWithText
    [ ("navLink", "/dashboards/" `T.append` k)
    , ("navName", dashName d)
    , ("navAlt",  dashDesc d)
    ]

dashSplices :: (MonadSnap m, MonadSnap (HeistT m m)) => Dash -> Time -> [(Text, Splice m)]
dashSplices d from =
     [ ("dashName", textSplice $ dashName d)
     , ("dashDesc", textSplice $ dashDesc d)
     , ("from",     textSplice . T.pack $ show from)
     , ("graphs",   graphSplices d)
     , ("fromLink", fromLinkSplice from)
     ]

fromLinkSplice :: (MonadSnap m, MonadSnap (HeistT m m)) => Time -> Splice m
fromLinkSplice from = do
    url  <- liftSnap $ currentUrl
    node <- getParamNode
    let span = fromString . T.unpack . fromJust $ X.getAttribute "span" node
        text = fromJust $ X.getAttribute "text" node
    return [active span from $ X.Element "li" [] [
        X.Element "a" [("href", T.concat [url, "?from=", T.pack $ show span])] $ [X.TextNode text]
        ]]
  where
    active a b n = if a == b
        then X.setAttribute "class" "active" n
        else n

graphSplices :: (MonadSnap m, MonadSnap (HeistT m m)) => Dash -> Splice m
graphSplices d = do
    ps <- (map parseParam . X.elementAttrs) `liftM` getParamNode
    flip mapSplices indexes $ \(i, (k, Graph{..})) -> runChildrenWith $
        [ ("graphNum",    textSplice . T.pack $ show i)
        , ("graphActive", textSplice $ if i == 0 then "active" else "")
        , ("graphName",   textSplice graphName)
        , ("graphDesc",   textSplice graphDesc)
        , ("graphUrl",    textSplice $ graphUrl d k ps)
        , ("fields",      fieldSplices graphFields)
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
    , T.pack . fromFragments $ pack ps
    ]

fieldSplices :: Monad m => [Field] -> Splice m
fieldSplices fs = flip mapSplices fs $ \Field{..} -> runChildrenWith $
    [ ("fieldAlias", textSplice fieldAlias)
    , ("fieldColor", textSplice fieldColor)
    ]
