-- |
-- Module      : Graphics.NanoSvg.Color
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Colors and paints.
--
-- Supports @none@, @transparent@ and @currentColor@; the
-- hexadecimal forms @#rgb@, @#rgba@, @#rrggbb@ and @#rrggbbaa@; the
-- functions @rgb()@, @rgba()@, @hsl()@ and @hsla()@, with their arguments
-- separated by commas or by spaces and an alpha after a slash as CSS Color
-- 4 writes them; and all 148 named CSS colors. Names are case-insensitive.
-- Hue accepts degrees only, with an optional @deg@ suffix.
--
-- Paint servers (@url()@), @inherit@ and system colors are unsupported.
-- 'parsePaint' and 'parseColor' return 'Nothing' for invalid or unsupported
-- values and require the whole input, apart from surrounding whitespace.
module Graphics.NanoSvg.Color
  ( -- * Paints
    paint
  , parsePaint

    -- * Colors
  , color
  , parseColor

    -- * Named colors
  , namedColor
  , namedColors
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import FlatParse.Basic qualified as F
import Graphics.NanoSvg.Internal.Parser
import Graphics.NanoSvg.Number (number)
import Graphics.NanoSvg.Types (Paint (..), RGBA, rgba)

--------------------------------------------------------------------------------
-- Paints
--------------------------------------------------------------------------------

-- | Parse a paint with optional surrounding whitespace. Both @none@ and
-- @transparent@ become 'PaintNone'; @currentColor@ becomes 'PaintCurrent'.
paint :: P Paint
paint = skipWsp *> value <* skipWsp
  where
    value =
      F.withOption
        (skipSatisfyByte (== 0x23))
        (const (PaintColor <$> hexColor))
        (named keyword (fmap PaintColor . functional) (fmap PaintColor . namedColor))
    keyword = \case
      "none" -> Just PaintNone
      "transparent" -> Just PaintNone
      "currentcolor" -> Just PaintCurrent
      _ -> Nothing

-- | Parse a complete @fill@ or @stroke@ value. 'Nothing' lets the caller
-- ignore the declaration and retain the previous paint.
parsePaint :: ByteString -> Maybe Paint
parsePaint src = evaluate src paint

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

-- | Parse a hex, RGB, HSL or named color without surrounding whitespace.
-- Paint-only keywords (@none@, @transparent@, @currentColor@) are rejected.
color :: P RGBA
color =
  F.withOption
    (skipSatisfyByte (== 0x23))
    (const hexColor)
    (named (const Nothing) functional namedColor)

-- | Read and lowercase a name, then dispatch to a keyword, function or table.
named :: (ByteString -> Maybe a) -> (ByteString -> P a) -> (ByteString -> Maybe a) -> P a
named keyword call table = do
  name <- lowercase <$> takeWhileByte isAsciiAlpha
  if BS.null name
    then F.failed
    else case keyword name of
      Just a -> a <$ F.fails (skipSatisfyByte (== 0x28))
      -- An opening parenthesis distinguishes functions from named colors.
      Nothing ->
        F.withOption
          (skipSatisfyByte (== 0x28))
          (const (call name))
          (maybe F.failed pure (table name))

-- | Parse a complete color with optional surrounding whitespace.
-- Accepts the forms supported by 'color', not paint-only keywords.
parseColor :: ByteString -> Maybe RGBA
parseColor src = evaluate src (skipWsp *> color <* skipWsp)

-- | The digits after a @#@: three, four, six or eight of them, the short
-- forms doubling each digit so that @#f90@ is @#ff9900@.
hexColor :: P RGBA
hexColor = do
  ds <- takeWhileByte isHexDigit
  let at i = hexValue (BS.index ds i)
      wide i = at (2 * i) * 16 + at (2 * i + 1)
      short i = let v = at i in v * 16 + v
  case BS.length ds of
    3 -> pure (rgba (short 0) (short 1) (short 2) 0xFF)
    4 -> pure (rgba (short 0) (short 1) (short 2) (short 3))
    6 -> pure (rgba (wide 0) (wide 1) (wide 2) 0xFF)
    8 -> pure (rgba (wide 0) (wide 1) (wide 2) (wide 3))
    _ -> F.failed

-- | Parse a color function after its name and opening parenthesis.
functional :: ByteString -> P RGBA
functional name = do
  c <- case name of
    "rgb" -> rgbBody
    "rgba" -> rgbBody
    "hsl" -> hslBody
    "hsla" -> hslBody
    _ -> F.failed
  skipWsp
  skipSatisfyByte (== 0x29)
  pure c

rgbBody :: P RGBA
rgbBody = do
  skipWsp
  r <- channel
  argSep
  g <- channel
  argSep
  b <- channel
  a <- opacity
  pure (rgba r g b a)

hslBody :: P RGBA
hslBody = do
  skipWsp
  h <- hue
  argSep
  s <- percentage
  argSep
  l <- percentage
  a <- opacity
  let (r, g, b) = hslToRgb h s l
  pure (rgba r g b a)

-- | A number, scaled if a @%@ follows it: @full@ is what @100%@ stands for.
scaled :: Float -> P Float
scaled full = do
  x <- number
  F.withOption (skipSatisfyByte (== 0x25)) (const (pure (x * full / 100))) (pure x)

-- | One of @r@, @g@ or @b@: a number out of 255, or a percentage.
channel :: P Word8
channel = round8 <$> scaled 255

-- | Hue in degrees, optionally followed by @deg@.
hue :: P Float
hue = number <* F.optional_ (F.byteString "deg")

-- | Percentage converted to [0, 1]. Also accepts a bare number as a percentage.
percentage :: P Float
percentage = do
  x <- number
  F.optional_ (skipSatisfyByte (== 0x25))
  pure (clamp01 (x / 100))

-- | The alpha argument, if there is one. It may be introduced by a comma,
-- as @rgba()@ writes it, or by a slash, as CSS Color 4 does.
opacity :: P Word8
opacity = (argSep *> alpha) F.<|> pure 0xFF
  where
    alpha = round8 . (255 *) . clamp01 <$> scaled 1

-- | Whitespace, at most one comma or slash, whitespace.
argSep :: P ()
argSep = do
  skipWsp
  F.optional_ (skipSatisfyByte (\w -> w == 0x2C || w == 0x2F))
  skipWsp

--------------------------------------------------------------------------------
-- Named colors
--------------------------------------------------------------------------------

-- | Look up one of the CSS color keywords. The name must already be
-- lowercase.
namedColor :: ByteString -> Maybe RGBA
namedColor name = Map.lookup name namedColorMap

namedColorMap :: Map ByteString RGBA
namedColorMap = Map.fromList namedColors
{-# NOINLINE namedColorMap #-}

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | CSS Color 3's conversion, with hue wrapped into a turn and saturation
-- and lightness already clamped.
hslToRgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hslToRgb h s l = (round8 (255 * f 0), round8 (255 * f 8), round8 (255 * f 4))
  where
    h' = h / 30
    a = s * min l (1 - l)
    f n =
      let k = wrap12 (n + h')
       in l - a * max (-1) (min 1 (min (k - 3) (9 - k)))
    wrap12 x = x - 12 * fromIntegral (floor (x / 12) :: Int)

{-# INLINE round8 #-}
round8 :: Float -> Word8
round8 x = fromIntegral (max 0 (min 255 (round x :: Int)))

-- | The 148 named CSS colors, sorted by lowercase name. Excludes paint keywords.
namedColors :: [(ByteString, RGBA)]
namedColors =
  [ ("aliceblue", rgba 0xf0 0xf8 0xff 0xFF)
  , ("antiquewhite", rgba 0xfa 0xeb 0xd7 0xFF)
  , ("aqua", rgba 0x00 0xff 0xff 0xFF)
  , ("aquamarine", rgba 0x7f 0xff 0xd4 0xFF)
  , ("azure", rgba 0xf0 0xff 0xff 0xFF)
  , ("beige", rgba 0xf5 0xf5 0xdc 0xFF)
  , ("bisque", rgba 0xff 0xe4 0xc4 0xFF)
  , ("black", rgba 0x00 0x00 0x00 0xFF)
  , ("blanchedalmond", rgba 0xff 0xeb 0xcd 0xFF)
  , ("blue", rgba 0x00 0x00 0xff 0xFF)
  , ("blueviolet", rgba 0x8a 0x2b 0xe2 0xFF)
  , ("brown", rgba 0xa5 0x2a 0x2a 0xFF)
  , ("burlywood", rgba 0xde 0xb8 0x87 0xFF)
  , ("cadetblue", rgba 0x5f 0x9e 0xa0 0xFF)
  , ("chartreuse", rgba 0x7f 0xff 0x00 0xFF)
  , ("chocolate", rgba 0xd2 0x69 0x1e 0xFF)
  , ("coral", rgba 0xff 0x7f 0x50 0xFF)
  , ("cornflowerblue", rgba 0x64 0x95 0xed 0xFF)
  , ("cornsilk", rgba 0xff 0xf8 0xdc 0xFF)
  , ("crimson", rgba 0xdc 0x14 0x3c 0xFF)
  , ("cyan", rgba 0x00 0xff 0xff 0xFF)
  , ("darkblue", rgba 0x00 0x00 0x8b 0xFF)
  , ("darkcyan", rgba 0x00 0x8b 0x8b 0xFF)
  , ("darkgoldenrod", rgba 0xb8 0x86 0x0b 0xFF)
  , ("darkgray", rgba 0xa9 0xa9 0xa9 0xFF)
  , ("darkgreen", rgba 0x00 0x64 0x00 0xFF)
  , ("darkgrey", rgba 0xa9 0xa9 0xa9 0xFF)
  , ("darkkhaki", rgba 0xbd 0xb7 0x6b 0xFF)
  , ("darkmagenta", rgba 0x8b 0x00 0x8b 0xFF)
  , ("darkolivegreen", rgba 0x55 0x6b 0x2f 0xFF)
  , ("darkorange", rgba 0xff 0x8c 0x00 0xFF)
  , ("darkorchid", rgba 0x99 0x32 0xcc 0xFF)
  , ("darkred", rgba 0x8b 0x00 0x00 0xFF)
  , ("darksalmon", rgba 0xe9 0x96 0x7a 0xFF)
  , ("darkseagreen", rgba 0x8f 0xbc 0x8f 0xFF)
  , ("darkslateblue", rgba 0x48 0x3d 0x8b 0xFF)
  , ("darkslategray", rgba 0x2f 0x4f 0x4f 0xFF)
  , ("darkslategrey", rgba 0x2f 0x4f 0x4f 0xFF)
  , ("darkturquoise", rgba 0x00 0xce 0xd1 0xFF)
  , ("darkviolet", rgba 0x94 0x00 0xd3 0xFF)
  , ("deeppink", rgba 0xff 0x14 0x93 0xFF)
  , ("deepskyblue", rgba 0x00 0xbf 0xff 0xFF)
  , ("dimgray", rgba 0x69 0x69 0x69 0xFF)
  , ("dimgrey", rgba 0x69 0x69 0x69 0xFF)
  , ("dodgerblue", rgba 0x1e 0x90 0xff 0xFF)
  , ("firebrick", rgba 0xb2 0x22 0x22 0xFF)
  , ("floralwhite", rgba 0xff 0xfa 0xf0 0xFF)
  , ("forestgreen", rgba 0x22 0x8b 0x22 0xFF)
  , ("fuchsia", rgba 0xff 0x00 0xff 0xFF)
  , ("gainsboro", rgba 0xdc 0xdc 0xdc 0xFF)
  , ("ghostwhite", rgba 0xf8 0xf8 0xff 0xFF)
  , ("gold", rgba 0xff 0xd7 0x00 0xFF)
  , ("goldenrod", rgba 0xda 0xa5 0x20 0xFF)
  , ("gray", rgba 0x80 0x80 0x80 0xFF)
  , ("green", rgba 0x00 0x80 0x00 0xFF)
  , ("greenyellow", rgba 0xad 0xff 0x2f 0xFF)
  , ("grey", rgba 0x80 0x80 0x80 0xFF)
  , ("honeydew", rgba 0xf0 0xff 0xf0 0xFF)
  , ("hotpink", rgba 0xff 0x69 0xb4 0xFF)
  , ("indianred", rgba 0xcd 0x5c 0x5c 0xFF)
  , ("indigo", rgba 0x4b 0x00 0x82 0xFF)
  , ("ivory", rgba 0xff 0xff 0xf0 0xFF)
  , ("khaki", rgba 0xf0 0xe6 0x8c 0xFF)
  , ("lavender", rgba 0xe6 0xe6 0xfa 0xFF)
  , ("lavenderblush", rgba 0xff 0xf0 0xf5 0xFF)
  , ("lawngreen", rgba 0x7c 0xfc 0x00 0xFF)
  , ("lemonchiffon", rgba 0xff 0xfa 0xcd 0xFF)
  , ("lightblue", rgba 0xad 0xd8 0xe6 0xFF)
  , ("lightcoral", rgba 0xf0 0x80 0x80 0xFF)
  , ("lightcyan", rgba 0xe0 0xff 0xff 0xFF)
  , ("lightgoldenrodyellow", rgba 0xfa 0xfa 0xd2 0xFF)
  , ("lightgray", rgba 0xd3 0xd3 0xd3 0xFF)
  , ("lightgreen", rgba 0x90 0xee 0x90 0xFF)
  , ("lightgrey", rgba 0xd3 0xd3 0xd3 0xFF)
  , ("lightpink", rgba 0xff 0xb6 0xc1 0xFF)
  , ("lightsalmon", rgba 0xff 0xa0 0x7a 0xFF)
  , ("lightseagreen", rgba 0x20 0xb2 0xaa 0xFF)
  , ("lightskyblue", rgba 0x87 0xce 0xfa 0xFF)
  , ("lightslategray", rgba 0x77 0x88 0x99 0xFF)
  , ("lightslategrey", rgba 0x77 0x88 0x99 0xFF)
  , ("lightsteelblue", rgba 0xb0 0xc4 0xde 0xFF)
  , ("lightyellow", rgba 0xff 0xff 0xe0 0xFF)
  , ("lime", rgba 0x00 0xff 0x00 0xFF)
  , ("limegreen", rgba 0x32 0xcd 0x32 0xFF)
  , ("linen", rgba 0xfa 0xf0 0xe6 0xFF)
  , ("magenta", rgba 0xff 0x00 0xff 0xFF)
  , ("maroon", rgba 0x80 0x00 0x00 0xFF)
  , ("mediumaquamarine", rgba 0x66 0xcd 0xaa 0xFF)
  , ("mediumblue", rgba 0x00 0x00 0xcd 0xFF)
  , ("mediumorchid", rgba 0xba 0x55 0xd3 0xFF)
  , ("mediumpurple", rgba 0x93 0x70 0xdb 0xFF)
  , ("mediumseagreen", rgba 0x3c 0xb3 0x71 0xFF)
  , ("mediumslateblue", rgba 0x7b 0x68 0xee 0xFF)
  , ("mediumspringgreen", rgba 0x00 0xfa 0x9a 0xFF)
  , ("mediumturquoise", rgba 0x48 0xd1 0xcc 0xFF)
  , ("mediumvioletred", rgba 0xc7 0x15 0x85 0xFF)
  , ("midnightblue", rgba 0x19 0x19 0x70 0xFF)
  , ("mintcream", rgba 0xf5 0xff 0xfa 0xFF)
  , ("mistyrose", rgba 0xff 0xe4 0xe1 0xFF)
  , ("moccasin", rgba 0xff 0xe4 0xb5 0xFF)
  , ("navajowhite", rgba 0xff 0xde 0xad 0xFF)
  , ("navy", rgba 0x00 0x00 0x80 0xFF)
  , ("oldlace", rgba 0xfd 0xf5 0xe6 0xFF)
  , ("olive", rgba 0x80 0x80 0x00 0xFF)
  , ("olivedrab", rgba 0x6b 0x8e 0x23 0xFF)
  , ("orange", rgba 0xff 0xa5 0x00 0xFF)
  , ("orangered", rgba 0xff 0x45 0x00 0xFF)
  , ("orchid", rgba 0xda 0x70 0xd6 0xFF)
  , ("palegoldenrod", rgba 0xee 0xe8 0xaa 0xFF)
  , ("palegreen", rgba 0x98 0xfb 0x98 0xFF)
  , ("paleturquoise", rgba 0xaf 0xee 0xee 0xFF)
  , ("palevioletred", rgba 0xdb 0x70 0x93 0xFF)
  , ("papayawhip", rgba 0xff 0xef 0xd5 0xFF)
  , ("peachpuff", rgba 0xff 0xda 0xb9 0xFF)
  , ("peru", rgba 0xcd 0x85 0x3f 0xFF)
  , ("pink", rgba 0xff 0xc0 0xcb 0xFF)
  , ("plum", rgba 0xdd 0xa0 0xdd 0xFF)
  , ("powderblue", rgba 0xb0 0xe0 0xe6 0xFF)
  , ("purple", rgba 0x80 0x00 0x80 0xFF)
  , ("rebeccapurple", rgba 0x66 0x33 0x99 0xFF)
  , ("red", rgba 0xff 0x00 0x00 0xFF)
  , ("rosybrown", rgba 0xbc 0x8f 0x8f 0xFF)
  , ("royalblue", rgba 0x41 0x69 0xe1 0xFF)
  , ("saddlebrown", rgba 0x8b 0x45 0x13 0xFF)
  , ("salmon", rgba 0xfa 0x80 0x72 0xFF)
  , ("sandybrown", rgba 0xf4 0xa4 0x60 0xFF)
  , ("seagreen", rgba 0x2e 0x8b 0x57 0xFF)
  , ("seashell", rgba 0xff 0xf5 0xee 0xFF)
  , ("sienna", rgba 0xa0 0x52 0x2d 0xFF)
  , ("silver", rgba 0xc0 0xc0 0xc0 0xFF)
  , ("skyblue", rgba 0x87 0xce 0xeb 0xFF)
  , ("slateblue", rgba 0x6a 0x5a 0xcd 0xFF)
  , ("slategray", rgba 0x70 0x80 0x90 0xFF)
  , ("slategrey", rgba 0x70 0x80 0x90 0xFF)
  , ("snow", rgba 0xff 0xfa 0xfa 0xFF)
  , ("springgreen", rgba 0x00 0xff 0x7f 0xFF)
  , ("steelblue", rgba 0x46 0x82 0xb4 0xFF)
  , ("tan", rgba 0xd2 0xb4 0x8c 0xFF)
  , ("teal", rgba 0x00 0x80 0x80 0xFF)
  , ("thistle", rgba 0xd8 0xbf 0xd8 0xFF)
  , ("tomato", rgba 0xff 0x63 0x47 0xFF)
  , ("turquoise", rgba 0x40 0xe0 0xd0 0xFF)
  , ("violet", rgba 0xee 0x82 0xee 0xFF)
  , ("wheat", rgba 0xf5 0xde 0xb3 0xFF)
  , ("white", rgba 0xff 0xff 0xff 0xFF)
  , ("whitesmoke", rgba 0xf5 0xf5 0xf5 0xFF)
  , ("yellow", rgba 0xff 0xff 0x00 0xFF)
  , ("yellowgreen", rgba 0x9a 0xcd 0x32 0xFF)
  ]
