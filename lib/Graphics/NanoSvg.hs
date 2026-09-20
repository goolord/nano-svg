{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.NanoSvg
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Read an SVG document into a flat array of shapes.
-- For geometry renderers, rasterizers and UI toolkits drawing static SVGs.
-- Shapes contain absolute path segments, a transform and a resolved style.
-- Rendering and viewport mapping are left to the caller.
--
-- > import Graphics.NanoSvg
-- > import qualified Data.ByteString as BS
-- >
-- > main :: IO ()
-- > main = do
-- >   bytes <- BS.readFile "clock.svg"
-- >   case parseSvg bytes of
-- >     Left err -> putStrLn err
-- >     Right doc -> print (documentSize doc, length (documentShapes doc))
--
-- = Supported SVG
--
-- The elements @svg@, @g@, @a@, @switch@, @use@, @path@, @rect@, @circle@,
-- @ellipse@, @line@, @polyline@ and @polygon@, with @use@ resolved against
-- @defs@ and @symbol@ by @id@.
--
-- The presentation properties @fill@, @stroke@, @stroke-width@,
-- @stroke-linecap@, @stroke-linejoin@, @stroke-miterlimit@, @fill-rule@,
-- @opacity@, @fill-opacity@, @stroke-opacity@, @display@ and @visibility@,
-- written either as attributes or in a @style@ attribute, which wins.
--
-- @transform@ in all six of its forms, colors in all of theirs (see
-- "Graphics.NanoSvg.Color"), and lengths in absolute units (see
-- "Graphics.NanoSvg.Number").
--
-- = Limitations
--
-- Gradients, patterns and other paint servers; @text@; @clipPath@, @mask@
-- and @filter@; @marker@; CSS in a @style@ element or an external sheet,
-- and so @class@ selectors; and animation are not supported.
-- The renderer must handle @preserveAspectRatio@.
--
-- Unknown elements and their children are skipped. Group opacity is
-- multiplied into each shape, not preserved as a compositing group.
-- Nested @svg@ and referenced @symbol@ elements do not establish viewports.
module Graphics.NanoSvg
  ( -- * Parsing
    parseSvg

    -- * Documents
  , Document (..)
  , documentWidth
  , documentHeight

    -- * Shapes
  , Shape (..)
  , Segment (..)
  , Style (..)
  , defaultStyle

    -- * Paint
  , Paint (..)
  , FillRule (..)
  , LineCap (..)
  , LineJoin (..)
  , RGBA (..)
  , rgba
  , rgbaR
  , rgbaG
  , rgbaB
  , rgbaA
  , withAlpha
  , black
  , transparent

    -- * Geometry
  , Point (..)
  , Box (..)
  , Matrix (..)
  , identity
  , multiply
  , transformPoint
  , averageScale

    -- * Standalone parsers
    -- $pieces
  )
where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Graphics.NanoSvg.Color (parsePaint)
import Graphics.NanoSvg.Internal.Parser (clamp01, lowercase, strip)
import Graphics.NanoSvg.Number (Length (..), Unit (..), parseLength, parseNumberList, parsePointList, parseUserUnits, toUserUnits)
import Graphics.NanoSvg.Path
import Graphics.NanoSvg.Types
import Graphics.NanoSvg.Xml

-- $pieces
--
-- These modules can be used independently:
-- "Graphics.NanoSvg.Xml" for the tree, "Graphics.NanoSvg.Path" for @d@ and
-- @transform@, "Graphics.NanoSvg.Color" for paints and
-- "Graphics.NanoSvg.Number" for numbers and lengths.

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse UTF-8 SVG bytes. Returns 'Left' on XML parse failure or when no
-- top-level @svg@ element exists. Invalid attribute values are ignored;
-- malformed paths retain the segments parsed before the error.
--
-- Accepts a UTF-8 byte-order mark and a DOCTYPE. No prior text decoding is
-- needed. The resulting t'Document' contains values, not source-buffer slices.
parseSvg :: ByteString -> Either String Document
parseSvg src = do
  elements <- parseXml src
  root <- case find ((== "svg") . elementName) elements of
    Just r -> Right r
    Nothing -> Left "no svg element"
  let (box, extent) = geometry root
      shapes = smallArrayFromList (collect (identified root) Set.empty identity defaultStyle root)
  pure
    Document
      { documentViewBox = box
      , documentSize = extent
      , documentShapes = shapes
      , documentKey = hashBytes src
      , documentMonochrome = all monochrome shapes
      }

-- | Resolve the root viewBox and output size. Missing dimensions use the
-- viewBox extent; without a valid viewBox, each missing dimension is 24.
geometry :: Element -> (Box, (Float, Float))
geometry root = (box, (fromMaybe (boxW box) width, fromMaybe (boxH box) height))
  where
    side k = attribute k root >>= parseUserUnits
    width = side "width"
    height = side "height"
    box = case maybe [] parseNumberList (attribute "viewBox" root) of
      [x, y, w, h] | w > 0, h > 0 -> Box x y w h
      -- Default to a 24-unit extent for each unresolved dimension.
      _ -> Box 0 0 (fromMaybe 24 width) (fromMaybe 24 height)

-- | Unspecified paints and @currentColor@ follow the caller's tint; one
-- explicit color anywhere makes the drawing multicolored.
monochrome :: Shape -> Bool
monochrome sh = tintable (styleFill s) && tintable (styleStroke s)
  where
    s = shapeStyle sh
    tintable = \case
      Just (PaintColor _) -> False
      _ -> True

-- | FNV-1a over the source, for callers that cache what they draw.
hashBytes :: ByteString -> Int
hashBytes = fromIntegral . BS.foldl' step (0xcbf29ce484222325 :: Word64)
  where
    step !h !w = (h `xor` fromIntegral w) * 0x100000001b3

--------------------------------------------------------------------------------
-- The tree
--------------------------------------------------------------------------------

-- | Index all IDs, including those under @defs@. The first duplicate wins.
identified :: Element -> Map ByteString Element
identified root =
  Map.fromListWith
    (\_new old -> old)
    [(i, el) | el <- descendants root, Just i <- [attribute "id" el]]

-- | IDs currently being expanded by @use@. Tracking active IDs stops cycles
-- before branching references can cause exponential expansion.
type Open = Set ByteString

-- | The shapes an element and its descendants draw, in paint order.
collect ::
  Map ByteString Element -> Open -> Matrix -> Style -> Element -> [Shape]
collect defs open outer inherited el = case elementName el of
  "svg" -> drawable descend
  "g" -> drawable descend
  "a" -> drawable descend
  -- Conditional processing attributes are ignored: use the first child
  -- that produces shapes.
  "switch" ->
    drawable (fromMaybe [] (find (not . null) (map into (elementChildren el))))
  "use" -> drawable used
  "path" -> shape (parsePath (value "d"))
  "rect" ->
    shape
      ( rectSegments
          (num "x")
          (num "y")
          (num "width")
          (num "height")
          (len "rx")
          (len "ry")
      )
  "circle" -> shape (circleSegments (num "cx") (num "cy") (num "r"))
  "ellipse" -> shape (ellipseSegments (num "cx") (num "cy") (num "rx") (num "ry"))
  "line" -> shape (lineSegments (num "x1") (num "y1") (num "x2") (num "y2"))
  "polyline" -> shape (polySegments False (parsePointList (value "points")))
  "polygon" -> shape (polySegments True (parsePointList (value "points")))
  -- Skip unsupported elements and their subtrees.
  _ -> []
  where
    -- Inline styles follow presentation attributes so later declarations win.
    properties =
      [(attributeName a, attributeValue a) | a <- elementAttributes el]
        <> styleProperties (value "style")

    -- Check visibility before evaluating geometry.
    drawable shapes = if hidden then [] else shapes
    hidden = uncurry (||) (foldl' seen (False, False) properties)
      where
        -- Last one wins, so a later declaration can un-hide as well as hide.
        seen acc@(off, invisible) (k, v) = case k of
          "display" -> (keyword v == "none", invisible)
          "visibility" -> (off, keyword v `elem` ["hidden", "collapse"])
          _ -> acc
        keyword = lowercase . strip

    transform = maybe outer (multiply outer . parseTransform) (attribute "transform" el)

    -- Resolve local opacity, then multiply by the ancestor product.
    -- This approximates group opacity; overlapping shapes are not composited
    -- as a group.
    style = own {styleOpacity = styleOpacity inherited * styleOpacity own}
      where
        own = foldl' applyProperty inherited {styleOpacity = 1} properties

    into = collect defs open transform style
    descend = concatMap into (elementChildren el)
    shape segs
      | hidden = []
      | null segs = []
      | otherwise = [Shape (smallArrayFromList segs) transform style]

    value k = fromMaybe "" (attribute k el)
    len k = attribute k el >>= parseUserUnits
    num k = fromMaybe 0 (len k)

    -- Apply the use element's offset and style to its local reference.
    -- For symbol/svg targets, expand only their children; target attributes
    -- and viewport mapping are skipped.
    used = case reference of
      Nothing -> []
      Just (key, target)
        | key `Set.member` open -> [] -- Already drawing this one.
        | elementName target `elem` ["symbol", "svg"] ->
            concatMap (deeper key) (elementChildren target)
        | otherwise -> deeper key target
      where
        reference = do
          href <- attribute "href" el
          key <- BS.stripPrefix "#" (strip href)
          target <- Map.lookup key defs
          pure (key, target)
        deeper key =
          collect
            defs
            (Set.insert key open)
            (transform `multiply` translation (num "x") (num "y"))
            style

--------------------------------------------------------------------------------
-- Presentation properties
--------------------------------------------------------------------------------

-- | Parse and clamp opacity to [0, 1]. Percentages are divided by 100;
-- other accepted lengths use the same conversion as geometry attributes.
opacityValue :: ByteString -> Maybe Float
opacityValue v =
  clamp01 <$> case parseLength v of
    Just (Length x Percent) -> Just (x / 100)
    Just l -> toUserUnits l
    Nothing -> Nothing

-- | Split inline declarations at semicolons and the first colon.
-- Entries without a colon are dropped. This is not a full CSS parser.
styleProperties :: ByteString -> [(ByteString, ByteString)]
styleProperties =
  concatMap declaration . BC.split ';'
  where
    declaration d = case BC.break (== ':') d of
      (k, v)
        | BS.null v -> []
        | otherwise -> [(lowercase (strip k), strip (BS.drop 1 v))]

-- | Apply a supported declaration. Invalid or unsupported values, including
-- @inherit@ and @url()@ paints, leave the current style unchanged.
applyProperty :: Style -> (ByteString, ByteString) -> Style
applyProperty s (k, raw) = case k of
  "opacity" -> maybe s (\o -> s {styleOpacity = o}) (opacityValue v)
  "fill" -> maybe s (\p -> s {styleFill = Just p}) (parsePaint v)
  "stroke" -> maybe s (\p -> s {styleStroke = Just p}) (parsePaint v)
  "stroke-width" -> maybe s (\w -> s {styleStrokeWidth = max 0 w}) (parseUserUnits v)
  "stroke-miterlimit" -> maybe s (\l -> s {styleMiterLimit = max 1 l}) (parseUserUnits v)
  "fill-opacity" -> maybe s (\o -> s {styleFillOpacity = o}) (opacityValue v)
  "stroke-opacity" -> maybe s (\o -> s {styleStrokeOpacity = o}) (opacityValue v)
  "stroke-linecap" -> case keyword of
    "butt" -> s {styleCap = CapButt}
    "round" -> s {styleCap = CapRound}
    "square" -> s {styleCap = CapSquare}
    _ -> s
  "stroke-linejoin" -> case keyword of
    "miter" -> s {styleJoin = JoinMiter}
    "round" -> s {styleJoin = JoinRound}
    "bevel" -> s {styleJoin = JoinBevel}
    _ -> s
  "fill-rule" -> case keyword of
    "nonzero" -> s {styleFillRule = NonZero}
    "evenodd" -> s {styleFillRule = EvenOdd}
    _ -> s
  _ -> s
  where
    v = strip raw
    keyword = lowercase v
