{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Ligature.Config.JSON
-- Copyright   : (c) 2012 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.Config.JSON (
      parse
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson          (eitherDecode)
import Data.Either         (partitionEithers)
import Data.List           (insert, isSuffixOf)
import Ligature.Types
import System.FilePath
import System.Directory

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as H
import qualified Data.Text                  as T

ext :: String
ext = ".json"

parse :: FilePath -> IO (HashMap Dash)
parse dir = do
    (es, ds) <- partitionEithers . map decode <$> load dir
    mapM_ putStrLn es
    return $ H.fromList ds

decode :: (FilePath, BL.ByteString) -> Either String (Key, Dash)
decode (path, bstr) = f $ eitherDecode bstr
  where
    f (Left e)  = Left $ path ++ ": " ++ e
    f (Right d) = Right (textKey . T.pack $ takeBaseName path, d)

load :: FilePath -> IO [(FilePath, BL.ByteString)]
load dir = files dir >>= mapM (f . joinPath . flip insert [dir])
  where
    f x = (x,) `liftM` BL.readFile x

files :: FilePath -> IO [FilePath]
files dir = filter (ext `isSuffixOf`) <$> getDirectoryContents dir
