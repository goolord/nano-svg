{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Graphics.NanoSvg.Types
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- The flattened document model: shapes in paint order, each with absolute
-- segments, a transform into the viewBox and a resolved style.
module Graphics.NanoSvg.Types
  ( -- * Colors
    RGBA (..)
  , rgba
  , black

    -- * Geometry
  , Point (..)
  , Box (..)
  , Matrix (..)
  , identity
  , multiply
  , translate
  , transformPoint
  , averageScale

    -- * Shapes
  , Segment (..)
  , Paint (..)
  , FillRule (..)
  , LineCap (..)
  , LineJoin (..)
  , Style (..)
  , defaultStyle
  , Shape (..)
  , Document (..)
  )
where

import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word32, Word8)
import Text.Printf (printf)

-- | A color packed as @0xRRGGBBAA@, not premultiplied. Shown as @#rrggbbaa@.
newtype RGBA = RGBA {rgbaToWord32 :: Word32}
  deriving newtype (Eq, Ord)

instance Show RGBA where
  show (RGBA w) = printf "#%08x" w

-- | Pack red, green, blue and alpha; alpha 255 is opaque.
rgba :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
rgba r g b a = RGBA (foldl' (\acc c -> acc * 256 + fromIntegral c) 0 [r, g, b, a])

-- | Opaque black, the initial value of @fill@.
black :: RGBA
black = rgba 0 0 0 255

data Point = Point !Float !Float
  deriving stock (Eq, Show)

-- | A @viewBox@.
data Box = Box {boxX, boxY, boxW, boxH :: !Float}
  deriving stock (Eq, Show)

-- | @matrix(a b c d e f)@, mapping @(x, y)@ to @(a*x + c*y + e, b*x + d*y + f)@.
data Matrix = Matrix {matrixA, matrixB, matrixC, matrixD, matrixE, matrixF :: !Float}
  deriving stock (Eq, Show)

identity :: Matrix
identity = Matrix 1 0 0 1 0 0

-- | @multiply outer inner@ applies @inner@ first.
multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix a b c d e f) (Matrix a' b' c' d' e' f') =
  Matrix (a * a' + c * b') (b * a' + d * b') (a * c' + c * d') (b * c' + d * d') (a * e' + c * f' + e) (b * e' + d * f' + f)

transformPoint :: Matrix -> Point -> Point
transformPoint (Matrix a b c d e f) (Point x y) = Point (a * x + c * y + e) (b * x + d * y + f)

-- | Square root of the absolute determinant: a single scale factor for
-- stroke widths, exact for uniform scaling.
averageScale :: Matrix -> Float
averageScale (Matrix a b c d _ _) = sqrt (abs (a * d - b * c))

-- | @translate(x, y)@.
translate :: Float -> Float -> Matrix
translate = Matrix 1 0 0 1

-- | A path command in absolute, shape-local coordinates. A renderer still
-- tracks the current point and subpath start.
data Segment
  = MoveTo !Point
  | LineTo !Point
  | -- | Two control points and the endpoint.
    CubicTo !Point !Point !Point
  | -- | Control point and endpoint.
    QuadTo !Point !Point
  | -- | Radii, x-axis rotation in degrees, large-arc and sweep flags, endpoint.
    ArcTo !Float !Float !Float !Bool !Bool !Point
  | ClosePath
  deriving stock (Eq, Show)

-- | 'PaintCurrent' is @currentColor@; @none@ and @transparent@ are 'PaintNone'.
data Paint = PaintNone | PaintCurrent | PaintColor !RGBA
  deriving stock (Eq, Show)

data FillRule = NonZero | EvenOdd
  deriving stock (Eq, Show)

data LineCap = CapButt | CapRound | CapSquare
  deriving stock (Eq, Show)

data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving stock (Eq, Show)

-- | Resolved presentation properties. An unset ('Nothing') fill is distinct
-- from black so a renderer can tint icons; see 'documentMonochrome'.
data Style = Style
  { styleFill :: !(Maybe Paint)
  , styleStroke :: !(Maybe Paint)
  , styleStrokeWidth :: !Float
  -- ^ Before 'shapeTransform' is applied.
  , styleCap :: !LineCap
  , styleJoin :: !LineJoin
  , styleMiterLimit :: !Float
  , styleFillRule :: !FillRule
  , styleOpacity :: !Float
  -- ^ Multiplied down the tree, not composited as a group.
  , styleFillOpacity :: !Float
  , styleStrokeOpacity :: !Float
  }
  deriving stock (Eq, Show)

-- | Unset fill, no stroke, width 1, butt caps, miter joins, miter limit 4,
-- nonzero fill rule and full opacity.
defaultStyle :: Style
defaultStyle = Style Nothing (Just PaintNone) 1 CapButt JoinMiter 4 NonZero 1 1 1

data Shape = Shape
  { shapeSegments :: !(SmallArray Segment)
  , shapeTransform :: !Matrix
  -- ^ From the segments' coordinates into 'documentViewBox'.
  , shapeStyle :: !Style
  }
  deriving stock (Eq, Show)

-- | A parsed document. Equality compares only 'documentKey'.
data Document = Document
  { documentViewBox :: !Box
  -- ^ The root @viewBox@, or else a box of the root size, 24 per missing side.
  , documentSize :: !(Float, Float)
  -- ^ The root @width@ and @height@, each defaulting to the viewBox's.
  , documentShapes :: !(SmallArray Shape)
  -- ^ In paint order.
  , documentKey :: !Int
  -- ^ FNV-1a hash of the source, for caching.
  , documentMonochrome :: !Bool
  -- ^ No shape has a literal 'PaintColor', so the drawing can be tinted.
  }

instance Eq Document where
  a == b = documentKey a == documentKey b

instance Show Document where
  show doc = "<svg " <> show (documentSize doc) <> ">"
