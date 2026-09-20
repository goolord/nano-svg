{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Graphics.NanoSvg.Types
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- The flattened document model used by "Graphics.NanoSvg".
--
-- A t'Document' holds t'Shape's in paint order. Each shape has a path in local
-- coordinates, a t'Matrix' mapping it into the document viewBox, and a
-- resolved t'Style'. Viewport mapping and rendering are the caller's job.
module Graphics.NanoSvg.Types
  ( -- * Colors
    RGBA (..)
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

    -- * Transforms
  , Matrix (..)
  , identity
  , multiply
  , translation
  , scaling
  , rotation
  , transformPoint
  , averageScale

    -- * Path segments
  , Segment (..)

    -- * Paint
  , Paint (..)
  , FillRule (..)
  , LineCap (..)
  , LineJoin (..)
  , Style (..)
  , defaultStyle

    -- * Shapes
  , Shape (..)

    -- * Documents
  , Document (..)
  , documentWidth
  , documentHeight
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (intToDigit)
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word32, Word8)

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

-- | An opaque or translucent color, packed as @0xRRGGBBAA@.
-- Channels are not premultiplied by alpha.
newtype RGBA = RGBA {rgbaToWord32 :: Word32}
  deriving newtype (Eq, Ord)

-- | Shown as @#rrggbbaa@.
instance Show RGBA where
  show c = '#' : concatMap hex2 [rgbaR c, rgbaG c, rgbaB c, rgbaA c]
    where
      hex2 w = [digit (w `shiftR` 4), digit (w .&. 0xF)]
      digit = intToDigit . fromIntegral

-- | Pack red, green, blue and alpha. Alpha 0 is transparent, 255 opaque.
{-# INLINE rgba #-}
rgba :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
rgba r g b a =
  RGBA $
    (w32 r `shiftL` 24)
      .|. (w32 g `shiftL` 16)
      .|. (w32 b `shiftL` 8)
      .|. w32 a
  where
    w32 = fromIntegral :: Word8 -> Word32

-- | The red channel.
{-# INLINE rgbaR #-}
rgbaR :: RGBA -> Word8
rgbaR (RGBA w) = fromIntegral (w `shiftR` 24)

-- | The green channel.
{-# INLINE rgbaG #-}
rgbaG :: RGBA -> Word8
rgbaG (RGBA w) = fromIntegral (w `shiftR` 16)

-- | The blue channel.
{-# INLINE rgbaB #-}
rgbaB :: RGBA -> Word8
rgbaB (RGBA w) = fromIntegral (w `shiftR` 8)

-- | The alpha channel.
{-# INLINE rgbaA #-}
rgbaA :: RGBA -> Word8
rgbaA (RGBA w) = fromIntegral w

-- | The color with its alpha replaced.
{-# INLINE withAlpha #-}
withAlpha :: Word8 -> RGBA -> RGBA
withAlpha a (RGBA w) = RGBA ((w .&. 0xFFFFFF00) .|. fromIntegral a)

-- | Opaque black, the initial value of @fill@.
black :: RGBA
black = rgba 0 0 0 255

-- | Zero in every channel.
transparent :: RGBA
transparent = RGBA 0

--------------------------------------------------------------------------------
-- Geometry
--------------------------------------------------------------------------------

-- | A point in user space.
data Point = Point {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving stock (Eq, Show)

-- | A rectangle as @x y width height@, the shape of a @viewBox@.
data Box = Box
  { boxX :: {-# UNPACK #-} !Float
  , boxY :: {-# UNPACK #-} !Float
  , boxW :: {-# UNPACK #-} !Float
  , boxH :: {-# UNPACK #-} !Float
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Transforms
--------------------------------------------------------------------------------

-- | SVG affine transform @matrix(a b c d e f)@:
--
-- > | a c e |
-- > | b d f |
-- > | 0 0 1 |
--
-- Maps @(x, y)@ to @(a*x + c*y + e, b*x + d*y + f)@.
data Matrix = Matrix
  { matrixA :: {-# UNPACK #-} !Float
  , matrixB :: {-# UNPACK #-} !Float
  , matrixC :: {-# UNPACK #-} !Float
  , matrixD :: {-# UNPACK #-} !Float
  , matrixE :: {-# UNPACK #-} !Float
  , matrixF :: {-# UNPACK #-} !Float
  }
  deriving stock (Eq, Show)

-- | Identity transform.
identity :: Matrix
identity = Matrix 1 0 0 1 0 0

-- | Compose transforms: @multiply outer inner@ applies @inner@ first.
{-# INLINE multiply #-}
multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix a b c d e f) (Matrix a' b' c' d' e' f') =
  Matrix
    (a * a' + c * b')
    (b * a' + d * b')
    (a * c' + c * d')
    (b * c' + d * d')
    (a * e' + c * f' + e)
    (b * e' + d * f' + f)

-- | @translate(x, y)@.
{-# INLINE translation #-}
translation :: Float -> Float -> Matrix
translation x y = Matrix 1 0 0 1 x y

-- | @scale(sx, sy)@.
{-# INLINE scaling #-}
scaling :: Float -> Float -> Matrix
scaling sx sy = Matrix sx 0 0 sy 0 0

-- | @rotate(a)@, the angle in degrees, clockwise because y points down.
{-# INLINE rotation #-}
rotation :: Float -> Matrix
rotation deg =
  let r = deg * pi / 180
      s = sin r
      c = cos r
   in Matrix c s (negate s) c 0 0

-- | Map a point through a transform.
{-# INLINE transformPoint #-}
transformPoint :: Matrix -> Point -> Point
transformPoint (Matrix a b c d e f) (Point x y) =
  Point (a * x + c * y + e) (b * x + d * y + f)

-- | Square root of the absolute determinant. Exact as a length scale for
-- uniform scaling; an area-based approximation for nonuniform scaling or
-- shear. Useful when a renderer represents stroke width with one scalar.
{-# INLINE averageScale #-}
averageScale :: Matrix -> Float
averageScale (Matrix a b c d _ _) = sqrt (abs (a * d - b * c))

--------------------------------------------------------------------------------
-- Path segments
--------------------------------------------------------------------------------

-- | A path command in absolute, shape-local coordinates. Relative commands
-- and shorthands are resolved during parsing; 'shapeTransform' is not baked
-- into the points. A renderer still tracks the current point and subpath start.
data Segment
  = -- | Start a new subpath.
    MoveTo !Point
  | -- | Straight line to a point.
    LineTo !Point
  | -- | Cubic Bezier: first control point, second control point, endpoint.
    CubicTo !Point !Point !Point
  | -- | Quadratic Bezier: control point, endpoint.
    QuadTo !Point !Point
  | -- | Elliptical arc: radii, the x-axis rotation in degrees, the
    --     large-arc and sweep flags, and the endpoint.
    ArcTo
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Bool
      !Bool
      !Point
  | -- | Close the current subpath back to where it started.
    ClosePath
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Paint
--------------------------------------------------------------------------------

-- | What fills or strokes a shape.
data Paint
  = -- | @none@: draw nothing.
    PaintNone
  | -- | @currentColor@: a color supplied by the renderer.
    PaintCurrent
  | -- | A literal color.
    PaintColor !RGBA
  deriving stock (Eq, Show)

-- | Rule for determining a path's filled interior.
data FillRule = NonZero | EvenOdd
  deriving stock (Eq, Show)

-- | Stroke endpoint shape.
data LineCap = CapButt | CapRound | CapSquare
  deriving stock (Eq, Show)

-- | Stroke corner shape.
data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving stock (Eq, Show)

-- | Resolved presentation properties in shape-local units.
--
-- An unset fill is distinct from explicit black so a renderer can apply an
-- icon tint. Without a tint, use SVG's default black fill. The default stroke
-- is 'Just' 'PaintNone'. See 'defaultStyle' and 'documentMonochrome'.
data Style = Style
  { styleFill :: !(Maybe Paint)
  -- ^ Fill paint; 'Nothing' means unspecified.
  , styleStroke :: !(Maybe Paint)
  -- ^ Stroke paint; 'Nothing' means unspecified.
  , styleStrokeWidth :: {-# UNPACK #-} !Float
  -- ^ Stroke width before 'shapeTransform' is applied.
  , styleCap :: !LineCap
  , styleJoin :: !LineJoin
  , styleMiterLimit :: {-# UNPACK #-} !Float
  -- ^ Maximum miter length as a multiple of stroke width.
  , styleFillRule :: !FillRule
  , styleOpacity :: {-# UNPACK #-} !Float
  -- ^ Local opacity multiplied by ancestor opacities. This does not preserve
  --   SVG group compositing for overlapping shapes.
  , styleFillOpacity :: {-# UNPACK #-} !Float
  -- ^ Fill opacity, multiplied with paint alpha and 'styleOpacity' when drawn.
  , styleStrokeOpacity :: {-# UNPACK #-} !Float
  -- ^ Stroke opacity, multiplied with paint alpha and 'styleOpacity' when drawn.
  }
  deriving stock (Eq, Show)

-- | Initial style: unset fill, no stroke, width 1, butt caps, miter joins,
-- miter limit 4, nonzero fill rule and full opacity.
defaultStyle :: Style
defaultStyle =
  Style
    { styleFill = Nothing
    , styleStroke = Just PaintNone
    , styleStrokeWidth = 1
    , styleCap = CapButt
    , styleJoin = JoinMiter
    , styleMiterLimit = 4
    , styleFillRule = NonZero
    , styleOpacity = 1
    , styleFillOpacity = 1
    , styleStrokeOpacity = 1
    }

--------------------------------------------------------------------------------
-- Shapes
--------------------------------------------------------------------------------

-- | One drawable path.
data Shape = Shape
  { shapeSegments :: !(SmallArray Segment)
  -- ^ Absolute segments in the shape's own coordinates.
  , shapeTransform :: !Matrix
  -- ^ From those coordinates into the document's 'documentViewBox'.
  , shapeStyle :: !Style
  -- ^ Presentation properties after inheritance and local declarations.
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Documents
--------------------------------------------------------------------------------

-- | A parsed SVG document. Equality compares only 'documentKey', not geometry.
data Document = Document
  { documentViewBox :: !Box
  -- ^ Root @viewBox@ when valid. Otherwise, a box at the origin using root
  --   dimensions, with 24 for each missing or unresolvable dimension.
  , documentSize :: {-# UNPACK #-} !(Float, Float)
  -- ^ Requested width and height in user units. Each unresolved root
  --   dimension falls back to the corresponding 'documentViewBox' extent.
  , documentShapes :: !(SmallArray Shape)
  -- ^ Every shape, in paint order.
  , documentKey :: {-# UNPACK #-} !Int
  -- ^ FNV-1a hash of the source bytes, converted to 'Int'. A cache hint, not
  --   a collision-free identifier; its width depends on the platform.
  , documentMonochrome :: !Bool
  -- ^ No shape has a literal 'PaintColor'. Unset paints, 'PaintNone' and
  --   'PaintCurrent' allow caller tinting. Explicit black counts as a literal
  --   color; an empty document is monochrome.
  }

-- | Documents are equal when their sources hash the same.
instance Eq Document where
  a == b = documentKey a == documentKey b

instance Show Document where
  show doc = "<svg " <> show (documentSize doc) <> ">"

-- | The width the document asks to be drawn at.
documentWidth :: Document -> Float
documentWidth = fst . documentSize

-- | The height the document asks to be drawn at.
documentHeight :: Document -> Float
documentHeight = snd . documentSize
