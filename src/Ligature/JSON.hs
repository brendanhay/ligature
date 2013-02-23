-- |
-- Module      : Ligature.JSON
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.JSON (
      palettes
    , dashboards
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson.Types
import Data.Either         (partitionEithers)
import Data.List           (insert, isSuffixOf)
import Data.Maybe
import Ligature.Types
import System.Directory
import System.FilePath

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as H
import qualified Data.Text                  as T

palettes :: FilePath -> IO [Palette]
palettes dir = map snd <$> loadFiles dir parseJSON

dashboards :: FilePath -> [Palette] -> IO (HashMap Dash)
dashboards dir ps = H.fromList <$> loadFiles dir (parseDashboard ps)

loadFiles :: FilePath -> (Value -> Parser a) -> IO [(Key, a)]
loadFiles dir p = do
    fs <- readFiles dir
    when (length fs == 0)
      (error $ "Empty directory: " ++ dir)
    (es, as) <- partitionEithers . map (parseFile p) <$> readFiles dir
    mapM putStrLn es
    return as

parseFile :: (Value -> Parser a) -> (FilePath, BL.ByteString) -> Either String (Key, a)
parseFile p (path, bstr) = case parseEither p obj of
    Left  e -> Left $ path ++ ": " ++ e
    Right d -> Right (textKey . T.pack $ takeBaseName path, d)
  where
    obj = maybe (error $ "Failed to decode: " ++ BL.unpack bstr)
        id (A.decode bstr)

readFiles :: FilePath -> IO [(FilePath, BL.ByteString)]
readFiles dir = jsonFiles dir >>= mapM (g . f)
  where
    f x = joinPath [dir, x]
    g x = (x,) `liftM` BL.readFile x

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles dir = filter (".json" `isSuffixOf`) <$> getDirectoryContents dir
