{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.NanoSvg.Attribute
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Parsers for individual SVG attribute values. Each accepts surrounding
-- whitespace, and returns 'Nothing' (or an empty or identity result) for
-- values it cannot read.
module Graphics.NanoSvg.Attribute
  ( parseNumber
  , parseNumberList
  , parsePoints
  , parseLength
  , parseOpacity
  , parsePath
  , parseTransform
  , parsePaint
  , parseColor
  )
where

import Control.Applicative (empty, many, optional, (<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isSpace, toLower)
import Data.Fixed (mod')
import FlatParse.Basic qualified as F
import GHC.Float (double2Float)
import Graphics.NanoSvg.Types

type P = F.Parser ()

-- | Run a parser over a whole value with optional surrounding whitespace.
whole :: P a -> ByteString -> Maybe a
whole p s = case F.runParser (wsp *> p <* wsp <* F.eof) s of
  F.OK a _ -> Just a
  _ -> Nothing

-- | Run a parser over a prefix of a value, ignoring the rest.
prefix :: a -> P a -> ByteString -> a
prefix def p s = case F.runParser p s of
  F.OK a _ -> a
  _ -> def

wsp, sep :: P ()
wsp = F.skipMany (F.skipSatisfyAscii isSpace)
sep = F.skipMany (F.skipSatisfyAscii (\c -> isSpace c || c == ','))

sym :: Char -> P ()
sym c = F.skipSatisfyAscii (== c)

letter :: Char -> Bool
letter c = isAsciiLower c || isAsciiUpper c

letters :: P ByteString
letters = F.byteStringOf (F.skipSome (F.skipSatisfyAscii letter))

word :: P ByteString
word = lower <$> letters

lower :: ByteString -> ByteString
lower = BC.map toLower

-- | A finite number, as in @1@, @-.5@, @1.@ or @1.5e+2@.
parseNumber :: ByteString -> Maybe Float
parseNumber = whole number

number :: P Float
number = do
  neg <- minus
  int <- digits
  frac <- (sym '.' *> digits) <|> pure BS.empty
  guard (not (BS.null int && BS.null frac))
  -- An @e@ without digits after it belongs to a unit, as in @1em@.
  e <- (F.skipSatisfyAscii (`elem` ['e', 'E']) *> power) <|> pure 0
  let x = double2Float (BS.foldl' step (BS.foldl' step 0 int) frac * 10 ^^ (e - BS.length frac))
  guard (not (isNaN x || isInfinite x))
  pure (if neg then negate x else x)
  where
    minus = (True <$ sym '-') <|> (False <$ sym '+') <|> pure False
    digits = F.byteStringOf (F.skipMany (F.skipSatisfyAscii isDigit))
    step acc d = acc * 10 + fromIntegral (d - 48) :: Double
    power = do
      neg <- minus
      k <- BS.foldl' (\acc d -> min 9999 (acc * 10 + fromIntegral d - 48)) 0 <$> F.byteStringOf (F.skipSome (F.skipSatisfyAscii isDigit))
      pure (if neg then negate k else k :: Int)

-- | Numbers separated by whitespace or commas, up to the first non-number.
parseNumberList :: ByteString -> [Float]
parseNumberList = prefix [] numbers

-- | Coordinate pairs, as in @points@, up to the first incomplete pair.
parsePoints :: ByteString -> [Point]
parsePoints = prefix [] (sep *> many (Point <$> arg <*> arg))

-- | An opacity or percentage, clamped to [0, 1].
parseOpacity :: ByteString -> Maybe Float
parseOpacity v = max 0 . min 1 <$> (whole ((/ 100) <$> number <* sym '%') v <|> parseLength v)

numbers :: P [Float]
numbers = sep *> many arg

arg :: P Float
arg = number <* sep

-- | A length in user units at 96 per inch. Relative units (@%@, @em@, @ex@)
-- give 'Nothing'.
parseLength :: ByteString -> Maybe Float
parseLength v = do
  (x, u) <- whole ((,) <$> number <*> (word <|> pure BS.empty)) v
  ($ x) <$> lookup u [("", id), ("px", id), ("pt", (/ 72) . (* 96)), ("pc", (* 16)), ("in", (* 96)), ("cm", (/ 2.54) . (* 96)), ("mm", (/ 25.4) . (* 96))]

-- | The @d@ attribute as absolute segments, keeping those before any error.
parsePath :: ByteString -> [Segment]
parsePath = prefix [] (path 'M' (Point 0 0) (MoveTo (Point 0 0)))

-- | The rest of a path, given the command to repeat, the subpath start and
-- the previous segment.
path :: Char -> Point -> Segment -> P [Segment]
path cmd !start !prev = sep *> ((F.satisfyAscii letter <* sep) <|> pure cmd) >>= \c -> step c <|> pure []
  where
    cur@(Point cx cy) = case prev of
      MoveTo p -> p
      LineTo p -> p
      CubicTo _ _ p -> p
      QuadTo _ p -> p
      ArcTo _ _ _ _ _ p -> p
      ClosePath -> start
    reflect (Point x y) = Point (2 * cx - x) (2 * cy - y)
    step c = case toLower c of
      -- Arguments after a moveto are linetos, and a closepath returns to the start.
      'm' -> pt >>= \p -> (MoveTo p :) <$> path (if rel then 'l' else 'L') p (MoveTo p)
      'z' -> (ClosePath :) <$> path (if rel then 'm' else 'M') start ClosePath
      'l' -> emit . LineTo =<< pt
      'h' -> emit . LineTo . (\x -> Point (if rel then cx + x else x) cy) =<< arg
      'v' -> emit . LineTo . (\y -> Point cx (if rel then cy + y else y)) =<< arg
      'c' -> emit =<< CubicTo <$> pt <*> pt <*> pt
      's' -> emit =<< CubicTo (case prev of CubicTo _ q _ -> reflect q; _ -> cur) <$> pt <*> pt
      'q' -> emit =<< QuadTo <$> pt <*> pt
      't' -> emit . QuadTo (case prev of QuadTo q _ -> reflect q; _ -> cur) =<< pt
      'a' -> emit =<< ArcTo <$> (abs <$> arg) <*> (abs <$> arg) <*> arg <*> flag <*> flag <*> pt
      _ -> empty
      where
        rel = isAsciiLower c
        pt = (\x y -> if rel then Point (cx + x) (cy + y) else Point x y) <$> arg <*> arg
        emit !seg = (seg :) <$> path c start seg
        -- Flags need no separator, as in @a1 1 0 00.5.5@.
        flag = (== '1') <$> F.satisfyAscii (`elem` ['0', '1']) <* sep

-- | A @transform@ list, the rightmost acting first. Unknown functions are the
-- identity, and malformed syntax ends the list.
parseTransform :: ByteString -> Matrix
parseTransform = prefix identity (foldl' multiply identity <$> (sep *> many item))
  where
    item = do
      name <- letters <* wsp <* sym '('
      args <- numbers <* wsp <* sym ')' <* sep
      pure case (name, args) of
        ("matrix", [a, b, c, d, e, f]) -> Matrix a b c d e f
        ("translate", [x]) -> translate x 0
        ("translate", [x, y]) -> translate x y
        ("scale", [k]) -> Matrix k 0 0 k 0 0
        ("scale", [x, y]) -> Matrix x 0 0 y 0 0
        ("rotate", [a]) -> rotate a
        ("rotate", [a, x, y]) -> translate x y `multiply` rotate a `multiply` translate (-x) (-y)
        ("skewX", [a]) -> Matrix 1 0 (tan (radians a)) 1 0 0
        ("skewY", [a]) -> Matrix 1 (tan (radians a)) 0 1 0 0
        _ -> identity
    radians a = a * pi / 180
    rotate a = let r = radians a in Matrix (cos r) (sin r) (-sin r) (cos r) 0 0

-- | A @fill@ or @stroke@ value. @url()@ and @inherit@ give 'Nothing'.
parsePaint :: ByteString -> Maybe Paint
parsePaint v = case lower (BC.strip v) of
  "none" -> Just PaintNone
  "transparent" -> Just PaintNone
  "currentcolor" -> Just PaintCurrent
  _ -> PaintColor <$> parseColor v

-- | A @#rgb@, @#rgba@, @#rrggbb@ or @#rrggbbaa@ color; @rgb()@, @rgba()@,
-- @hsl()@ or @hsla()@ with comma or space separators; or a named color.
parseColor :: ByteString -> Maybe RGBA
parseColor = whole color

color :: P RGBA
color = (sym '#' *> hex) <|> do
  name <- word
  (sym '(' *> wsp *> function name <* wsp <* sym ')') <|> maybe empty pure (lookup name namedColors)

hex :: P RGBA
hex = do
  ds <- map (fromIntegral . digitToInt) . BC.unpack <$> F.byteStringOf (F.skipMany (F.skipSatisfyAscii isHexDigit))
  let pairs = \case
        a : b : r -> a * 16 + b : pairs r
        _ -> []
  case if length ds `elem` [3, 4] then map (* 17) ds else if even (length ds) then pairs ds else [] of
    [r, g, b] -> pure (rgba r g b 255)
    [r, g, b, a] -> pure (rgba r g b a)
    _ -> empty

function :: ByteString -> P RGBA
function name
  | name `elem` ["rgb", "rgba"] = rgba <$> channel <*> (comma *> channel) <*> (comma *> channel) <*> alpha
  | name `elem` ["hsl", "hsla"] = do
      h <- number <* optional (F.byteString "deg")
      s <- comma *> percent
      l <- comma *> percent
      let a = s * min l (1 - l)
          f n = let k = mod' (n + h / 30) 12 in byte (255 * (l - a * max (-1) (min 1 (min (k - 3) (9 - k)))))
      rgba (f 0) (f 8) (f 4) <$> alpha
  | otherwise = empty
  where
    comma = wsp *> optional (F.skipSatisfyAscii (`elem` [',', '/'])) *> wsp
    scaled full = number >>= \x -> (x * full / 100 <$ sym '%') <|> pure x
    channel = byte <$> scaled 255
    percent = max 0 . min 1 . (/ 100) <$> number <* optional (sym '%')
    alpha = (comma *> (byte . (255 *) . max 0 . min 1 <$> scaled 1)) <|> pure 255
    byte x = fromIntegral (max 0 (min 255 (round x :: Int)))

-- | The CSS named colors, excluding the paint keywords.
namedColors :: [(ByteString, RGBA)]
namedColors = pairs (BC.words table)
  where
    pairs (n : c : r) = (n, prefix black hex c) : pairs r
    pairs _ = []
    table =
      "aliceblue f0f8ff antiquewhite faebd7 aqua 00ffff aquamarine 7fffd4 azure f0ffff \
      \beige f5f5dc bisque ffe4c4 black 000000 blanchedalmond ffebcd blue 0000ff \
      \blueviolet 8a2be2 brown a52a2a burlywood deb887 cadetblue 5f9ea0 chartreuse 7fff00 \
      \chocolate d2691e coral ff7f50 cornflowerblue 6495ed cornsilk fff8dc crimson dc143c \
      \cyan 00ffff darkblue 00008b darkcyan 008b8b darkgoldenrod b8860b darkgray a9a9a9 \
      \darkgreen 006400 darkgrey a9a9a9 darkkhaki bdb76b darkmagenta 8b008b darkolivegreen \
      \556b2f darkorange ff8c00 darkorchid 9932cc darkred 8b0000 darksalmon e9967a \
      \darkseagreen 8fbc8f darkslateblue 483d8b darkslategray 2f4f4f darkslategrey 2f4f4f \
      \darkturquoise 00ced1 darkviolet 9400d3 deeppink ff1493 deepskyblue 00bfff dimgray \
      \696969 dimgrey 696969 dodgerblue 1e90ff firebrick b22222 floralwhite fffaf0 \
      \forestgreen 228b22 fuchsia ff00ff gainsboro dcdcdc ghostwhite f8f8ff gold ffd700 \
      \goldenrod daa520 gray 808080 green 008000 greenyellow adff2f grey 808080 honeydew \
      \f0fff0 hotpink ff69b4 indianred cd5c5c indigo 4b0082 ivory fffff0 khaki f0e68c \
      \lavender e6e6fa lavenderblush fff0f5 lawngreen 7cfc00 lemonchiffon fffacd lightblue \
      \add8e6 lightcoral f08080 lightcyan e0ffff lightgoldenrodyellow fafad2 lightgray \
      \d3d3d3 lightgreen 90ee90 lightgrey d3d3d3 lightpink ffb6c1 lightsalmon ffa07a \
      \lightseagreen 20b2aa lightskyblue 87cefa lightslategray 778899 lightslategrey \
      \778899 lightsteelblue b0c4de lightyellow ffffe0 lime 00ff00 limegreen 32cd32 linen \
      \faf0e6 magenta ff00ff maroon 800000 mediumaquamarine 66cdaa mediumblue 0000cd \
      \mediumorchid ba55d3 mediumpurple 9370db mediumseagreen 3cb371 mediumslateblue \
      \7b68ee mediumspringgreen 00fa9a mediumturquoise 48d1cc mediumvioletred c71585 \
      \midnightblue 191970 mintcream f5fffa mistyrose ffe4e1 moccasin ffe4b5 navajowhite \
      \ffdead navy 000080 oldlace fdf5e6 olive 808000 olivedrab 6b8e23 orange ffa500 \
      \orangered ff4500 orchid da70d6 palegoldenrod eee8aa palegreen 98fb98 paleturquoise \
      \afeeee palevioletred db7093 papayawhip ffefd5 peachpuff ffdab9 peru cd853f pink \
      \ffc0cb plum dda0dd powderblue b0e0e6 purple 800080 rebeccapurple 663399 red ff0000 \
      \rosybrown bc8f8f royalblue 4169e1 saddlebrown 8b4513 salmon fa8072 sandybrown \
      \f4a460 seagreen 2e8b57 seashell fff5ee sienna a0522d silver c0c0c0 skyblue 87ceeb \
      \slateblue 6a5acd slategray 708090 slategrey 708090 snow fffafa springgreen 00ff7f \
      \steelblue 4682b4 tan d2b48c teal 008080 thistle d8bfd8 tomato ff6347 turquoise \
      \40e0d0 violet ee82ee wheat f5deb3 white ffffff whitesmoke f5f5f5 yellow ffff00 \
      \yellowgreen 9acd32"
