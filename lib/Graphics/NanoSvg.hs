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
-- 'encodeSvg' writes a document back out as SVG, one @path@ per shape.
--
-- = Supported SVG
--
-- The elements @svg@, @g@, @a@, @switch@, @use@, @path@, @rect@, @circle@,
-- @ellipse@, @line@, @polyline@ and @polygon@, with @use@ resolved by @id@
-- and nested at most 'maxUseDepth' deep.
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
  , maxUseDepth
  , encodeSvg
  , svgBuilder
  , module Graphics.NanoSvg.Types
  , module Graphics.NanoSvg.Attribute
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Bits (shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr, toLower)
import Data.Foldable (toList)
import Data.List (find, intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import FlatParse.Basic qualified as F
import Graphics.NanoSvg.Attribute
import Graphics.NanoSvg.Types
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
      shapes = collect ids Set.empty identity defaultStyle root []
  pure
    Document
      { documentViewBox = box
      , documentSize = (fromMaybe (boxW box) (side "width"), fromMaybe (boxH box) (side "height"))
      , documentShapes = smallArrayFromList shapes
      , documentKey = fromIntegral (BS.foldl' (\h w -> (h `xor` fromIntegral w) * 0x100000001b3) (0xcbf29ce484222325 :: Word64) src)
      , documentMonochrome = null [() | Shape _ _ s <- shapes, Just (PaintColor _) <- [styleFill s, styleStroke s]]
      }

-- | Prepend the shapes an element draws. @open@ holds the @use@ targets
-- being expanded, to cut reference cycles and bound nesting.
collect :: Map ByteString Element -> Set ByteString -> Matrix -> Style -> Element -> [Shape] -> [Shape]
collect ids open outer inherited el rest
  | keyword "display" == Just "none" || keyword "visibility" `elem` [Just "hidden", Just "collapse"] = rest
  | otherwise = case tag el of
      t | t `elem` ["svg", "g", "a"] -> foldr into rest (kids el)
      "switch" -> foldr (\kid next -> case into kid [] of [] -> next; drawn -> drawn <> rest) rest (kids el)
      "use" -> case attr "href" el >>= BS.stripPrefix "#" . BC.strip of
        Just key | Set.size open < maxUseDepth, Set.notMember key open, Just target <- Map.lookup key ids -> do
          let draw = collect ids (Set.insert key open) (transform `multiply` translate (num el "x") (num el "y")) style
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

-- | How many @use@ elements may nest, counting through their targets; one
-- nested deeper draws nothing. Firefox's @svg.use-element.recursive-clone-limit@
-- is also 8.
maxUseDepth :: Int
maxUseDepth = 8

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
-- Encoding
--------------------------------------------------------------------------------

-- | Write a document as UTF-8 SVG. 'parseSvg' reads the result back to the
-- same view box, size and shapes.
encodeSvg :: Document -> ByteString
encodeSvg = BS.toStrict . B.toLazyByteString . svgBuilder

-- | 'encodeSvg' as a 'B.Builder': one @path@ per shape, in paint order, with
-- its transform and each property that differs from 'defaultStyle'.
svgBuilder :: Document -> B.Builder
svgBuilder doc =
  "<svg xmlns=\"http://www.w3.org/2000/svg\""
    <> attribute "width" (number w)
    <> attribute "height" (number h)
    <> attribute "viewBox" (spaced (map number [x, y, bw, bh]))
    <> ">"
    <> foldMap shape (documentShapes doc)
    <> "</svg>"
  where
    Box x y bw bh = documentViewBox doc
    (w, h) = documentSize doc

shape :: Shape -> B.Builder
shape (Shape segs m s) =
  "<path"
    <> attribute "d" (spaced (map segment (toList segs)))
    <> (if m == identity then mempty else attribute "transform" (matrix m))
    <> foldMap (attribute "fill" . paint) (styleFill s)
    <> foldMap (attribute "fill-opacity" . number) (changed styleFillOpacity)
    <> foldMap (attribute "fill-rule" . \case NonZero -> "nonzero"; EvenOdd -> "evenodd") (changed styleFillRule)
    <> foldMap (attribute "stroke" . paint) (join (changed styleStroke))
    <> foldMap (attribute "stroke-opacity" . number) (changed styleStrokeOpacity)
    <> foldMap (attribute "stroke-width" . number) (changed styleStrokeWidth)
    <> foldMap (attribute "stroke-linecap" . \case CapButt -> "butt"; CapRound -> "round"; CapSquare -> "square") (changed styleCap)
    <> foldMap (attribute "stroke-linejoin" . \case JoinMiter -> "miter"; JoinRound -> "round"; JoinBevel -> "bevel") (changed styleJoin)
    <> foldMap (attribute "stroke-miterlimit" . number) (changed styleMiterLimit)
    <> foldMap (attribute "opacity" . number) (changed styleOpacity)
    <> "/>"
  where
    changed :: Eq a => (Style -> a) -> Maybe a
    changed field = if field s == field defaultStyle then Nothing else Just (field s)
    matrix (Matrix a b c d e f) = "matrix(" <> spaced (map number [a, b, c, d, e, f]) <> ")"

segment :: Segment -> B.Builder
segment = \case
  MoveTo p -> "M" <> point p
  LineTo p -> "L" <> point p
  CubicTo p q r -> "C" <> spaced [point p, point q, point r]
  QuadTo p q -> "Q" <> spaced [point p, point q]
  ArcTo rx ry angle large sweep p -> "A" <> spaced [number rx, number ry, number angle, flag large, flag sweep, point p]
  ClosePath -> "Z"
  where
    point (Point px py) = number px <> " " <> number py
    flag b = if b then "1" else "0"

-- | @#rrggbb@, or @#rrggbbaa@ when not opaque.
paint :: Paint -> B.Builder
paint = \case
  PaintNone -> "none"
  PaintCurrent -> "currentColor"
  PaintColor (RGBA c) -> "#" <> foldMap (B.word8HexFixed . channel) (if channel 0 == 255 then [24, 16, 8] else [24, 16, 8, 0])
    where
      channel k = fromIntegral (c `shiftR` k) :: Word8

-- | The shortest digits that read back as the same 'Float', and an integer
-- without a trailing @.0@.
number :: Float -> B.Builder
number v
  | abs v < 1e7, v == fromIntegral i = B.intDec i
  | otherwise = B.floatDec v
  where
    i = truncate v :: Int

attribute :: B.Builder -> B.Builder -> B.Builder
attribute k v = " " <> k <> "=\"" <> v <> "\""

spaced :: [B.Builder] -> B.Builder
spaced = mconcat . intersperse " "

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
-- else as written. Linear in the length of the value.
entities :: ByteString -> ByteString
entities v
  | BC.notElem '&' v = v
  | otherwise = BS.toStrict (B.toLazyByteString (go v))
  where
    go s = case BC.break (== '&') s of
      (a, b) | BS.null b -> B.byteString a
      (a, b) -> B.byteString a <> case BC.break (\c -> c == ';' || c == '&') (BS.drop 1 b) of
        -- A reference cannot contain an ampersand, so the scan stops at the
        -- next one instead of running on to a distant semicolon.
        (ref, rest) | Just rest' <- BC.stripPrefix ";" rest, Just c <- entity ref -> B.charUtf8 c <> go rest'
        _ -> B.char7 '&' <> go (BS.drop 1 b)
    entity ref = case BC.uncons ref of
      Just ('#', n) -> case BC.uncons n of
        Just (x, h) | toLower x == 'x' -> scalar F.anyAsciiHexInt h
        _ -> scalar F.anyAsciiDecimalInt n
      _ -> lookup ref [("amp", '&'), ("lt", '<'), ("gt", '>'), ("quot", '"'), ("apos", '\'')]
    -- The digit parsers fail on overflow rather than wrap around.
    scalar digits ds = case F.runParser (digits <* F.eof) ds of
      F.OK c _ | c > 0, c <= 0x10FFFF, c < 0xD800 || c > 0xDFFF -> Just (chr c)
      _ -> Nothing

