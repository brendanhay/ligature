-- |
-- Module      : Ligature.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan.g.hay@gmail.com>
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
    , Dash(..)
    , Graph(..)
    , Field(..)
    , Function(..)
    , Param(..)
    , byteKey
    , textKey
    , graphFill
    ) where

import Control.Applicative
import Data.Aeson.Types
import Data.Char            (toLower)
import Data.Conduit.List    (consume)
import Data.Text            (Text)
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit          as C
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E

type HashMap a = H.HashMap Text a

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

data Field = Field Text Text [Function] deriving (Show)

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
        [(k, v)] -> case k of
            "Width" -> Width <$> (parseJSON v)

        -- _              -> error "Ligature.Dashboards.Param expected one of: " ++ show
        --                   ["Width", "Height", "Template", "Margin", "BgColor",
        --                    "FgColor", "FontName", "FontSize", "FontBold", "FontItalic",
        --                    "YMin", "YMax", "ColorList", "Title", "VTitle", "LineMode",
        --                    "LineWidth", "HideLegend", "HideAxes", "HideGrid",
        --                    "MinXStep", "MajorGridLineColor", "MinorGridLineColor",
        --                    "MinorY", "Tz"]



-- parseParams :: HashMap Value -> Parser [Param]
-- parseParams hmap = parseJSON $ Object hmap

-- capitalise :: HashMap Value -> HashMap Value
-- capitalise = H.fromList . H.foldlWithKey' f []
--   where
--     f acc k v = (g k, v) : acc
--     g k       = toUpper (T.head k) `T.cons` T.tail k

   -- instance FromJSON Param where
   --    parseJSON
   --      = \ value_a3PA
   --          -> case value_a3PA of {
   --               Object obj_a3PB
   --                 -> case H.toList obj_a3PB of {
   --                      [(conKey_a3PC, conVal_a3PD)]
   --                        -> case conKey_a3PC of {
   --                             _ | (conKey_a3PC == (T.pack "Width"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PE -> (Width <$> (parseJSON arg_a3PE)) }
   --                               | (conKey_a3PC == (T.pack "Height"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PF -> (Height <$> (parseJSON arg_a3PF)) }
   --                               | (conKey_a3PC == (T.pack "Template"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PG -> (Template <$> (parseJSON arg_a3PG)) }
   --                               | (conKey_a3PC == (T.pack "Margin"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PH -> (Margin <$> (parseJSON arg_a3PH)) }
   --                               | (conKey_a3PC == (T.pack "BgColor"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PI -> (BgColor <$> (parseJSON arg_a3PI)) }
   --                               | (conKey_a3PC == (T.pack "FgColor"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PJ -> (FgColor <$> (parseJSON arg_a3PJ)) }
   --                               | (conKey_a3PC == (T.pack "FontName"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PK -> (FontName <$> (parseJSON arg_a3PK)) }
   --                               | (conKey_a3PC == (T.pack "FontSize"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PL -> (FontSize <$> (parseJSON arg_a3PL)) }
   --                               | (conKey_a3PC == (T.pack "FontBold"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PM -> (FontBold <$> (parseJSON arg_a3PM)) }
   --                               | (conKey_a3PC == (T.pack "FontItalic"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PN -> (FontItalic <$> (parseJSON arg_a3PN)) }
   --                               | (conKey_a3PC == (T.pack "YMin"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PO -> (YMin <$> (parseJSON arg_a3PO)) }
   --                               | (conKey_a3PC == (T.pack "YMax"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PP -> (YMax <$> (parseJSON arg_a3PP)) }
   --                               | (conKey_a3PC == (T.pack "ColorList"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PQ -> (ColorList <$> (parseJSON arg_a3PQ)) }
   --                               | (conKey_a3PC == (T.pack "Title"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PR -> (Title <$> (parseJSON arg_a3PR)) }
   --                               | (conKey_a3PC == (T.pack "VTitle"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PS -> (VTitle <$> (parseJSON arg_a3PS)) }
   --                               | (conKey_a3PC == (T.pack "LineMode"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PT -> (LineMode <$> (parseJSON arg_a3PT)) }
   --                               | (conKey_a3PC == (T.pack "LineWidth"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PU -> (LineWidth <$> (parseJSON arg_a3PU)) }
   --                               | (conKey_a3PC == (T.pack "HideLegend"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PV -> (HideLegend <$> (parseJSON arg_a3PV)) }
   --                               | (conKey_a3PC == (T.pack "HideAxes"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PW -> (HideAxes <$> (parseJSON arg_a3PW)) }
   --                               | (conKey_a3PC == (T.pack "HideGrid"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PX -> (HideGrid <$> (parseJSON arg_a3PX)) }
   --                               | (conKey_a3PC == (T.pack "MinXStep"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PY -> (MinXStep <$> (parseJSON arg_a3PY)) }
   --                               | (conKey_a3PC == (T.pack "MajorGridLineColor"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3PZ -> (MajorGridLineColor <$> (parseJSON arg_a3PZ)) }
   --                               | (conKey_a3PC == (T.pack "MinorGridLineColor"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3Q0 -> (MinorGridLineColor <$> (parseJSON arg_a3Q0)) }
   --                               | (conKey_a3PC == (T.pack "MinorY"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3Q1 -> (MinorY <$> (parseJSON arg_a3Q1)) }
   --                               | (conKey_a3PC == (T.pack "Tz"))
   --                               -> case conVal_a3PD of {
   --                                    arg_a3Q2 -> (Tz <$> (parseJSON arg_a3Q2)) }
   --                               | otherwise
   --                               -> Data.Aeson.TH.conNotFoundFail
   --                                    "Ligature.Dashboards.Param"
   --                                    ["Width", "Height", "Template", "Margin", "BgColor",
   --                                     "FgColor", "FontName", "FontSize", "FontBold", "FontItalic",
   --                                     "YMin", "YMax", "ColorList", "Title", "VTitle", "LineMode",
   --                                     "LineWidth", "HideLegend", "HideAxes", "HideGrid",
   --                                     "MinXStep", "MajorGridLineColor", "MinorGridLineColor",
   --                                     "MinorY", "Tz"]
   --                                    (T.unpack conKey_a3PC) }
   --                      other_a3Q3
   --                        -> Data.Aeson.TH.wrongPairCountFail
   --                             "Ligature.Dashboards.Param" ((show . length) other_a3Q3) }
   --               other_a3Q4
   --                 -> Data.Aeson.TH.noObjectFail
   --                      "Ligature.Dashboards.Param"
   --                      (Data.Aeson.TH.valueConName other_a3Q4) }

instance FromJSON [Param] where
    parseJSON (Object o) = mapM f $ H.toList o
      where
        f = parseJSON . Object . H.fromList . (:[])
        -- ^ There be much woe and misery

byteKey :: BS.ByteString -> Text
byteKey = textKey . E.decodeUtf8

textKey :: Text -> Text
textKey = T.map (toLower . f)
  where
    f ' ' = '-'
    f c   = c

stripKeys :: FromJSON a => HashMap Value -> [Text] -> Parser a
stripKeys hmap = parseJSON . Object . foldl (flip H.delete) hmap
