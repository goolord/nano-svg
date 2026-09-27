-- |
-- Module      : Graphics.NanoSvg
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Read an SVG document into a flat array of shapes, each with absolute path
-- segments, a transform and a resolved style. Rendering and viewport mapping
-- are left to the caller.
--
-- > case parseSvg bytes of
-- >   Left err -> putStrLn err
-- >   Right doc -> print (documentSize doc, length (documentShapes doc))
--
-- = Supported SVG
--
-- The elements @svg@, @g@, @a@, @switch@, @use@, @path@, @rect@, @circle@,
-- @ellipse@, @line@, @polyline@ and @polygon@, with @use@ resolved by @id@.
-- The properties @fill@, @stroke@, @stroke-width@, @stroke-linecap@,
-- @stroke-linejoin@, @stroke-miterlimit@, @fill-rule@, @opacity@,
-- @fill-opacity@, @stroke-opacity@, @display@ and @visibility@, as
-- attributes or in a @style@ attribute, which wins. All @transform@
-- functions; hex, @rgb()@, @hsl()@ and the 148 named colors; lengths in
-- absolute units at 96 per inch.
--
-- = Limitations
--
-- Paint servers, @text@, @clipPath@, @mask@, @filter@, @marker@, CSS
-- stylesheets and animation are not supported, and unknown elements are
-- skipped with their children. Group opacity is multiplied into each shape.
-- Nested @svg@ and @symbol@ do not establish viewports, and
-- @preserveAspectRatio@ is left to the renderer.
module Graphics.NanoSvg
  ( parseSvg
  , module Graphics.NanoSvg.Types
  , module Graphics.NanoSvg.Attribute
  )
where

import Control.Applicative ((<|>))
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr, toLower)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word64)
import Graphics.NanoSvg.Attribute
import Graphics.NanoSvg.Types
import Numeric (readDec, readHex)
import Text.XML.Hexml qualified as X

-- | Parse UTF-8 SVG bytes. Fails on malformed XML or without an @svg@
-- element. Invalid attribute values are ignored, and malformed paths keep
-- the segments before the error.
parseSvg :: ByteString -> Either String Document
parseSvg src = do
  roots <- either (Left . BC.unpack) (Right . nodes) (X.parse (withoutDoctype src))
  root <- maybe (Left "no svg element") Right (find ((== "svg") . tag) roots)
  let side k = attr k root >>= parseLength
      box = case maybe [] parseNumberList (attr "viewBox" root) of
        [x, y, w, h] | w > 0, h > 0 -> Box x y w h
        _ -> Box 0 0 (fromMaybe 24 (side "width")) (fromMaybe 24 (side "height"))
      -- The first element with an id wins.
      ids = Map.fromListWith (\_ first -> first) [(i, el) | el <- descendants root, Just i <- [attr "id" el]]
      shapes = collect ids [] identity defaultStyle root []
  pure
    Document
      { documentViewBox = box
      , documentSize = (fromMaybe (boxW box) (side "width"), fromMaybe (boxH box) (side "height"))
      , documentShapes = smallArrayFromList shapes
      , documentKey = fromIntegral (BS.foldl' (\h w -> (h `xor` fromIntegral w) * 0x100000001b3) (0xcbf29ce484222325 :: Word64) src)
      , documentMonochrome = null [() | Shape _ _ s <- shapes, Just (PaintColor _) <- [styleFill s, styleStroke s]]
      }

-- | Prepend the shapes an element draws. @open@ holds the @use@ targets
-- being expanded, to cut reference cycles.
collect :: Map ByteString Element -> [ByteString] -> Matrix -> Style -> Element -> [Shape] -> [Shape]
collect ids open outer inherited el rest
  | keyword "display" == Just "none" || keyword "visibility" `elem` [Just "hidden", Just "collapse"] = rest
  | otherwise = case tag el of
      t | t `elem` ["svg", "g", "a"] -> foldr into rest (kids el)
      "switch" -> foldr (\kid next -> case into kid [] of [] -> next; drawn -> drawn <> rest) rest (kids el)
      "use" -> case attr "href" el >>= BS.stripPrefix "#" . BC.strip of
        Just key | key `notElem` open, Just target <- Map.lookup key ids -> do
          let draw = collect ids (key : open) (transform `multiply` translate (num el "x") (num el "y")) style
          if tag target `elem` ["symbol", "svg"] then foldr draw rest (kids target) else draw target rest
        _ -> rest
      _ | null (geometry el) -> rest
        | otherwise -> Shape (geometry el) transform style : rest
  where
    props = attrs el <> declarations (value "style" el)
    latest = reverse props
    keyword k = lower . BC.strip <$> lookup k latest
    transform = maybe outer (multiply outer . parseTransform) (attr "transform" el)
    own = foldl' property inherited {styleOpacity = 1} props
    style = own {styleOpacity = styleOpacity inherited * styleOpacity own}
    into = collect ids open transform style

-- | The segments of a basic shape or path, in its own coordinates.
segments :: Element -> [Segment]
segments el = case tag el of
  "path" -> parsePath (value "d" el)
  "rect" -> rect (num el "x") (num el "y") (num el "width") (num el "height") (len el "rx") (len el "ry")
  "circle" -> ellipse (num el "cx") (num el "cy") (num el "r") (num el "r")
  "ellipse" -> ellipse (num el "cx") (num el "cy") (num el "rx") (num el "ry")
  "line" -> [MoveTo (Point (num el "x1") (num el "y1")), LineTo (Point (num el "x2") (num el "y2"))]
  "polyline" -> poly []
  "polygon" -> poly [ClosePath]
  _ -> []
  where
    poly close = case parsePoints (value "points" el) of
      [] -> []
      p : ps -> MoveTo p : map LineTo ps <> close

value :: ByteString -> Element -> ByteString
value k = fromMaybe "" . attr k

len :: Element -> ByteString -> Maybe Float
len el k = attr k el >>= parseLength

num :: Element -> ByteString -> Float
num el = fromMaybe 0 . len el

rect :: Float -> Float -> Float -> Float -> Maybe Float -> Maybe Float -> [Segment]
rect x y w h mrx mry
  | w <= 0 || h <= 0 = []
  | rx <= 0 || ry <= 0 = [MoveTo (Point x y), LineTo (Point (x + w) y), LineTo (Point (x + w) (y + h)), LineTo (Point x (y + h)), ClosePath]
  | otherwise =
      [ MoveTo (Point (x + rx) y)
      , LineTo (Point (x + w - rx) y)
      , corner (Point (x + w) (y + ry))
      , LineTo (Point (x + w) (y + h - ry))
      , corner (Point (x + w - rx) (y + h))
      , LineTo (Point (x + rx) (y + h))
      , corner (Point x (y + h - ry))
      , LineTo (Point x (y + ry))
      , corner (Point (x + rx) y)
      , ClosePath
      ]
  where
    -- A missing radius defaults to the other, capped at half the side.
    rx = min (w / 2) (abs (fromMaybe 0 (mrx <|> mry)))
    ry = min (h / 2) (abs (fromMaybe 0 (mry <|> mrx)))
    corner = ArcTo rx ry 0 False True

ellipse :: Float -> Float -> Float -> Float -> [Segment]
ellipse cx cy rx ry
  | rx <= 0 || ry <= 0 = []
  | otherwise = [MoveTo (Point (cx + rx) cy), arc (Point (cx - rx) cy), arc (Point (cx + rx) cy), ClosePath]
  where
    arc = ArcTo rx ry 0 False True

-- | Apply one declaration; invalid or unsupported values leave the style as is.
property :: Style -> (ByteString, ByteString) -> Style
property s (k, v) = fromMaybe s case k of
  "fill" -> (\p -> s {styleFill = Just p}) <$> parsePaint v
  "stroke" -> (\p -> s {styleStroke = Just p}) <$> parsePaint v
  "stroke-width" -> (\w -> s {styleStrokeWidth = max 0 w}) <$> parseLength v
  "stroke-miterlimit" -> (\l -> s {styleMiterLimit = max 1 l}) <$> parseLength v
  "opacity" -> (\o -> s {styleOpacity = o}) <$> parseOpacity v
  "fill-opacity" -> (\o -> s {styleFillOpacity = o}) <$> parseOpacity v
  "stroke-opacity" -> (\o -> s {styleStrokeOpacity = o}) <$> parseOpacity v
  "stroke-linecap" -> (\c -> s {styleCap = c}) <$> lookup kw [("butt", CapButt), ("round", CapRound), ("square", CapSquare)]
  "stroke-linejoin" -> (\j -> s {styleJoin = j}) <$> lookup kw [("miter", JoinMiter), ("round", JoinRound), ("bevel", JoinBevel)]
  "fill-rule" -> (\r -> s {styleFillRule = r}) <$> lookup kw [("nonzero", NonZero), ("evenodd", EvenOdd)]
  _ -> Nothing
  where
    kw = lower (BC.strip v)

-- | Split a @style@ attribute into declarations. Not a CSS parser.
declarations :: ByteString -> [(ByteString, ByteString)]
declarations s =
  [(lower (BC.strip k), BC.strip (BS.drop 1 v)) | d <- BC.split ';' s, let (k, v) = BC.break (== ':') d, not (BS.null v)]

lower :: ByteString -> ByteString
lower = BC.map toLower


--------------------------------------------------------------------------------
-- XML
--------------------------------------------------------------------------------

-- | 'geometry' is parsed on first draw and shared by every @use@ of the
-- element.
data Element = Element {tag :: ByteString, attrs :: [(ByteString, ByteString)], geometry :: SmallArray Segment, kids :: [Element]}

attr :: ByteString -> Element -> Maybe ByteString
attr k = lookup k . attrs

descendants :: Element -> [Element]
descendants el = go el []
  where
    go e rest = e : foldr go rest (kids e)

-- | Child elements, without the processing instructions hexml reports.
nodes :: X.Node -> [Element]
nodes n = [element c | c <- X.children n, not (BS.isPrefixOf "<?" (X.outer c))]
  where
    element c = el
      where
        el = Element (local (X.name c)) [(local k, entities v) | X.Attribute k v <- X.attributes c] (smallArrayFromList (segments el)) (nodes c)
    local s = maybe s (\i -> BS.drop (i + 1) s) (BC.elemIndexEnd ':' s)

-- | Remove a DOCTYPE, including an internal subset, which hexml rejects.
withoutDoctype :: ByteString -> ByteString
withoutDoctype s = before <> BC.drop 1 (BC.dropWhile (/= '>') decl)
  where
    (before, doctype) = BS.breakSubstring "<!DOCTYPE" s
    decl = if BC.elem '[' (BC.takeWhile (/= '>') doctype) then BC.dropWhile (/= ']') doctype else doctype

-- | Decode the predefined entities and character references; leave anything
-- else as written.
entities :: ByteString -> ByteString
entities v
  | BC.notElem '&' v = v
  | otherwise = BS.toStrict (B.toLazyByteString (go v))
  where
    go s = case BC.break (== '&') s of
      (a, b) | BS.null b -> B.byteString a
      (a, b) -> B.byteString a <> case BC.break (== ';') (BS.drop 1 b) of
        (ref, rest) | not (BS.null rest), Just c <- entity ref -> B.charUtf8 c <> go (BS.drop 1 rest)
        _ -> B.char7 '&' <> go (BS.drop 1 b)
    entity ref = case BC.unpack ref of
      "amp" -> Just '&'
      "lt" -> Just '<'
      "gt" -> Just '>'
      "quot" -> Just '"'
      "apos" -> Just '\''
      '#' : x : h | toLower x == 'x' -> scalar (readHex h)
      '#' : d -> scalar (readDec d)
      _ -> Nothing
    scalar :: [(Integer, String)] -> Maybe Char
    scalar = \case
      [(c, "")] | c > 0, c <= 0x10FFFF, c < 0xD800 || c > 0xDFFF -> Just (chr (fromInteger c))
      _ -> Nothing

