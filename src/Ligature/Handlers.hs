-- |
-- Module      : Ligature.Handlers
-- Copyright   : (c) 2012 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.Handlers (
      routes
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)
import Data.Maybe
import Ligature.Splices
import Ligature.Types
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as H

routes :: HasHeist a => HashMap Dash -> [(ByteString, Handler a b ())]
routes hmap =
    [ ("/dashboards/:dashboard/graphs/:graph", getGraph hmap)
    , ("/dashboards/:dashboard",               getDash hmap)
    ]

getGraph :: HasHeist a => HashMap Dash -> Handler a b ()
getGraph hmap = do
    d <- requireParam "dashboard"
    g <- requireParam "graph"
    x <- requireParam "width"
    y <- requireParam "height"
    let dash = hmap H.! (byteKey d)
        gs   = dashGraphs dash
        grph = gs H.! (byteKey g)
    liftIO (graphFill "https://graphite.z-infra.com" grph (read $ BS.unpack x) (read $ BS.unpack y)) >>= writeBS

getDash :: HasHeist a => HashMap Dash -> Handler a b ()
getDash hmap = do
    d <- requireParam "dashboard"
    renderWithSplices "dashboard" . dashSplices $ hmap H.! (byteKey d)

requireParam :: MonadSnap m => ByteString -> m ByteString
requireParam name = fromJust <$> getParam name
