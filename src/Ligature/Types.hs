-- |
-- Module      : Ligature.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.Types (
      HashMap
    , Key(..)
    , Dash(..)
    , Graph(..)
    , Field(..)
    , Function(..)
    , Param(..)
    , parseParam
    , byteKey
    , textKey
    , keyText
    , findDash
    , findGraph
    ) where

import Control.Applicative
import Control.Arrow          (second)
import Data.Aeson.Types
import Data.Attoparsec.Number (Number(..))
import Data.Char              (toUpper, toLower)
import Data.Hashable          (Hashable)
import Data.Maybe
import Data.String            (IsString(..))
import Data.Text              (Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E

type HashMap a = H.HashMap Key a

newtype Key = Key Text deriving (Ord, Eq, Hashable, Show)

instance IsString Key where
    fromString = Key . fromString

data Dash = Dash
    { dashName   :: Text
    , dashDesc   :: Text
    , dashGraphs :: HashMap Graph
    } deriving (Show)

instance FromJSON Dash where
    parseJSON (Object o) = Dash
        <$> o .: "name"
        <*> o .: "description"
        <*> (o .: "graphs" >>= return . graphs)
      where
        graphs = H.fromList . map (\g -> (textKey $ graphName g, g))
    parseJSON e = typeMismatch "Object" e

data Graph = Graph
    { graphName   :: Text
    , graphDesc   :: Text
    , graphParams :: [Param]
    , graphFields :: [Field]
    } deriving (Show)

instance FromJSON Graph where
    parseJSON (Object o) = Graph
        <$> o .: "name"
        <*> o .: "description"
        <*> o `stripKeys` ["name", "description", "fields"]
        <*> o .: "fields"
    parseJSON e = typeMismatch "Object" e

data Field = Field
    { fieldAlias   :: Text
    , fieldContext :: Text
    , fieldFuncs   :: [Function]
    } deriving (Show)

instance FromJSON Field where
    parseJSON (Object o) = Field
        <$> o .: "alias"
        <*> o .: "context"
        <*> o `stripKeys` ["alias", "context"]
    parseJSON e = typeMismatch "Object" e

data Function = Function Text Value deriving (Show)

instance FromJSON [Function] where
    parseJSON (Object o) = return $ H.foldlWithKey' f [] o
      where
        f acc k v = Function k v : acc
    parseJSON e = typeMismatch "Object" e

data Param
    = Width Int
    | Height Int
    | Template Text
    | Margin Int
    | BgColor Text
    | FgColor Text
    | FontName Text
    | FontSize Int
    | FontBold Bool
    | FontItalic Bool
    | YMin Double
    | YMax Double
    | ColorList [Text] -- ^ This should be added by the fields if it doesn't exist
    | Title Text
    | VTitle Text
    | LineMode Text
    | LineWidth Int
    | HideLegend Bool
    | HideAxes Bool
    | HideGrid Bool
    | MinXStep Int
    | MajorGridLineColor Text
    | MinorGridLineColor Text
    | MinorY Int
    | Tz Text
      deriving (Show)

instance FromJSON Param where
    parseJSON (Object o) = case H.toList o of
        [(k, v)] -> fromPair (k, v)
        _        -> error $ "Ligature.Dashboards.Param received malformed object"
    parseJSON e          = typeMismatch "Object" e

instance FromJSON [Param] where
    parseJSON (Object o) = sequence . map fromPair $ H.toList o
    parseJSON e          = typeMismatch "Object" e

parseParam :: (Text, Text) -> Param
parseParam = go String
  where
    go ctor t = case parse (fromPair . second ctor) t of
        Success v -> v
        -- Temoporary hilariously ridiculous work around
        Error "when expecting a Integral, encountered String instead" ->
            go (Number . I . f) t
        Error "when expecting a Double, encountered String instead" ->
            go (Number . D . f) t
        Error "when expecting a Bool, encountered String instead" ->
            go (Bool . f . \s -> toUpper (T.head s) `T.cons` T.tail s) t
        Error e ->
            error $ "Unable to parse param: " ++ e
      where
        f :: Read a => Text -> a
        f = fromMaybe (error $ "failed to read: " ++ show t) . readMay . T.unpack

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
    [x] -> Just x
    _   -> Nothing

fromPair :: Pair -> Parser Param
fromPair (k, v) = case k of
    "width"              -> f Width
    "height"             -> f Height
    "template"           -> f Template
    "margin"             -> f Margin
    "bgcolor"            -> f BgColor
    "fgcolor"            -> f FgColor
    "fontName"           -> f FontName
    "fontSize"           -> f FontSize
    "fontBold"           -> f FontBold
    "fontItalic"         -> f FontItalic
    "yMin"               -> f YMin
    "yMax"               -> f YMax
    "colorList"          -> f ColorList
    "title"              -> f Title
    "vtitle"             -> f VTitle
    "lineMode"           -> f LineMode
    "lineWidth"          -> f LineWidth
    "hideLegend"         -> f HideLegend
    "hideAxes"           -> f HideAxes
    "hideGrid"           -> f HideGrid
    "minXStep"           -> f MinXStep
    "majorGridLineColor" -> f MajorGridLineColor
    "minorGridLineColor" -> f MinorGridLineColor
    "minorY"             -> f MinorY
    "tz"                 -> f Tz
    _                    -> error $ "Ligature.Dashboards.Param doesn't support " ++ T.unpack k
  where
    f ctor = ctor <$> (parseJSON v)

byteKey :: BS.ByteString -> Key
byteKey = textKey . E.decodeUtf8

textKey :: Text -> Key
textKey = Key . T.map (toLower . f)
  where
    f ' ' = '-'
    f c   = c

keyText :: Key -> Text
keyText (Key k) = k

findDash :: HashMap Dash -> Key -> Dash
findDash m k = case H.lookup k m of
    Just v  -> v
    Nothing -> error $ "Dashboard key not found: " ++ show (k, H.keys m)

findGraph :: HashMap Dash -> Key -> Key -> Graph
findGraph m d g = case H.lookup g m' of
    Just v  -> v
    Nothing -> error $ "Graph key not found: " ++ show (g, H.keys m')
  where
    m' = dashGraphs $ findDash m d

stripKeys :: FromJSON a => H.HashMap Text Value -> [Text] -> Parser a
stripKeys hmap = parseJSON . Object . foldl (flip H.delete) hmap
