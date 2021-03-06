{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Ligature.URL
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Ligature.URL (
      UrlFragment(..)
    , fromFragments
    , pack
    , graphData
    ) where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Arrow            ((***), second)
import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson.Types
import Data.Char                (toLower)
import Data.Conduit.List        (consume)
import Data.Hashable            (Hashable)
import Data.List                (nub, foldl', intercalate)
import Data.List.Split          (splitOneOf)
import Data.Maybe               (fromJust)
import Data.String              (IsString(..))
import Data.Text                (Text)
import Ligature.Types
import Network.HTTP             (urlEncodeVars, urlEncode)
import Network.HTTP.Conduit
import Network.URI

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit          as C
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E

class UrlFragment a where
    toFragment :: a -> (Text, Text)

data Fragment = forall a. UrlFragment a => Fragment a

pack :: UrlFragment a => [a] -> [Fragment]
pack = map Fragment

instance UrlFragment Fragment where
    toFragment (Fragment f) = toFragment f

instance UrlFragment Field where
    toFragment (Field _ clr ctx) = ("target", t)
      where
        t = T.concat ["color(", ctx, ",\"", clr, "\")"]

instance UrlFragment Param where
    toFragment p = case p of
        (From v)               -> ("from", T.pack $ show v)
        (Width v)              -> ("width", s v)
        (Height v)             -> ("height", s v)
        (Template v)           -> ("template", v)
        (Margin v)             -> ("margin", s v)
        (BgColor v)            -> ("bgcolor", v)
        (FgColor v)            -> ("fgcolor", v)
        (FontName v)           -> ("fontName", v)
        (FontSize v)           -> ("fontSize", s v)
        (FontBold v)           -> ("fontBold", T.toLower $ s v)
        (FontItalic v)         -> ("fontItalic", T.toLower $ s v)
        (YMin v)               -> ("yMin", s v)
        (YMax v)               -> ("yMax", s v)
        (ColorList vs)         -> ("colorList", T.intercalate "," vs)
        (Title v)              -> ("title", v)
        (VTitle v)             -> ("vtitle", v)
        (LineMode v)           -> ("lineWode", v)
        (LineWidth v)          -> ("lineWidth", s v)
        (HideLegend v)         -> ("hideLegend", T.toLower $ s v)
        (HideAxes v)           -> ("hideAxes", T.toLower $ s v)
        (HideGrid v)           -> ("hideGrid", T.toLower $ s v)
        (MinXStep v)           -> ("minXStep", s v)
        (MajorGridLineColor v) -> ("majorGridLineColor", v)
        (MinorGridLineColor v) -> ("minorGridLineColor", v)
        (MinorY v)             -> ("minorY", s v)
        (Tz v)                 -> ("tz", v)
      where
        s :: Show a => a -> Text
        s = T.pack . show

fromFragments :: [Fragment] -> String
fromFragments = intercalate "&" . map (parts . toFragment)
  where
    parts (k, v) = concat [T.unpack k, "=", urlEncode . T.unpack $ v]

graphData :: URI -> Graph -> [Param] -> IO BS.ByteString
graphData uri g ps = withManager $ \m -> do
    req  <- parseUrl $ generateUrl uri g ps
    res  <- responseBody <$> http (applyAuth uri req) m
    body <- res C.$$+- consume
    return $ BS.concat body

generateUrl :: URI -> Graph -> [Param] -> String
generateUrl uri g ps =
    uriHost uri ++ "/render/?" ++ (fromFragments $ fields ++ params)
  where
    params = pack ps
    fields = pack $ graphFields g

applyAuth :: URI -> Request a -> Request a
applyAuth uri req = maybe req (\(u, p) -> applyBasicAuth u p req) (uriUser uri)

uriHost :: URI -> String
uriHost uri = uriScheme uri ++ "//" ++ uriRegName auth ++ uriPort auth
  where
    auth = uriAuth uri

uriUser :: URI -> Maybe (BS.ByteString, BS.ByteString)
uriUser uri = case uriUserInfo $ uriAuth uri of
    "" -> Nothing
    s  -> let (u:p:_) = splitOneOf ":@" s
          in  Just (BS.pack u, BS.pack p)

uriAuth :: URI -> URIAuth
uriAuth = fromJust . uriAuthority
