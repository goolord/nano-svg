{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.NanoSvg.Path
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Parse path data and transforms, and convert basic SVG shapes to paths.
--
-- Paths use absolute 'Segment's. Parsing resolves relative coordinates,
-- @H@, @V@, @S@, @T@ and implicit command repetition. Renderers still track
-- the current point and subpath start; transforms are returned separately.
--
-- Malformed path data retains the complete segments before the error.
module Graphics.NanoSvg.Path
  ( -- * Path data
    pathData
  , parsePath

    -- * Transforms
  , transformList
  , parseTransform

    -- * The shape elements
  , rectSegments
  , circleSegments
  , ellipseSegments
  , lineSegments
  , polySegments
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import FlatParse.Basic qualified as F
import Graphics.NanoSvg.Internal.Parser
import Graphics.NanoSvg.Number (coordinatePair, flag, number, numberList)
import Graphics.NanoSvg.Types

--------------------------------------------------------------------------------
-- Path data
--------------------------------------------------------------------------------

-- | The @d@ attribute of a @path@, as absolute segments.
--
-- Always succeeds, stopping at the first unknown or incomplete command.
pathData :: P [Segment]
pathData = walk (St 0x4D origin origin NoCtrl)
  where
    origin = Point 0 0

-- | Parse a @d@ attribute into absolute segments. Ignores a malformed tail;
-- returns @[]@ when no complete segment can be read.
parsePath :: ByteString -> [Segment]
parsePath src = maybe [] fst (runPartial src pathData)

-- | State carried between path commands.
data St = St
  { stCommand :: !Word8
  -- ^ The command to repeat when a run of arguments follows.
  , stCurrent :: !Point
  , stSubpath :: !Point
  -- ^ Where the open subpath began, for @Z@.
  , stControl :: !Ctrl
  }

-- | Previous control point: @S@ reflects only a cubic control point and @T@
-- only a quadratic one. Otherwise, the current point is used.
data Ctrl = NoCtrl | CubicCtrl !Point | QuadCtrl !Point

walk :: St -> P [Segment]
walk st = do
  skipWspComma
  peekByte >>= \case
    Nothing -> pure []
    Just w
      | isAsciiAlpha w -> F.skip 1 *> skipWspComma *> step st {stCommand = w}
      -- A run of arguments repeats the command that introduced it.
      | otherwise -> step st

-- | Read one argument group, stopping the path if it is incomplete.
step :: St -> P [Segment]
step st@(St cmd current@(Point cx cy) subpath ctrl) = case lower cmd of
  0x6D -> ending do
    -- m: moveto
    p <- point
    -- A second pair after a moveto is a lineto, and a relative moveto
    -- makes them relative linetos.
    (MoveTo p :)
      <$> walk
        st
          { stCommand = if relative then 0x6C else 0x4C
          , stCurrent = p
          , stSubpath = p
          , stControl = NoCtrl
          }
  0x6C -> ending do
    -- l: lineto
    p <- point
    (LineTo p :) <$> continue p NoCtrl
  0x68 -> ending do
    -- h: horizontal lineto
    x <- number
    let p = Point (if relative then cx + x else x) cy
    (LineTo p :) <$> continue p NoCtrl
  0x76 -> ending do
    -- v: vertical lineto
    y <- number
    let p = Point cx (if relative then cy + y else y)
    (LineTo p :) <$> continue p NoCtrl
  0x63 -> ending do
    -- c: curveto
    c1 <- point
    skipWspComma
    c2 <- point
    skipWspComma
    p <- point
    (CubicTo c1 c2 p :) <$> continue p (CubicCtrl c2)
  0x73 -> ending do
    -- s: smooth curveto
    c2 <- point
    skipWspComma
    p <- point
    let c1 = case ctrl of
          CubicCtrl q -> reflect current q
          _ -> current
    (CubicTo c1 c2 p :) <$> continue p (CubicCtrl c2)
  0x71 -> ending do
    -- q: quadratic curveto
    c1 <- point
    skipWspComma
    p <- point
    (QuadTo c1 p :) <$> continue p (QuadCtrl c1)
  0x74 -> ending do
    -- t: smooth quadratic curveto
    p <- point
    let c1 = case ctrl of
          QuadCtrl q -> reflect current q
          _ -> current
    (QuadTo c1 p :) <$> continue p (QuadCtrl c1)
  0x61 -> ending do
    -- a: elliptical arc
    rx <- number
    skipWspComma
    ry <- number
    skipWspComma
    rot <- number
    skipWspComma
    -- Single-digit flags need no separators: a1 1 0 00.5.5 has seven arguments.
    large <- flag
    skipWspComma
    sweep <- flag
    skipWspComma
    p <- point
    (ArcTo (abs rx) (abs ry) rot large sweep p :) <$> continue p NoCtrl
  0x7A ->
    -- z: closepath. The current point returns to where the subpath began,
    -- and a bare coordinate pair after it starts a new one.
    (ClosePath :)
      <$> walk
        st
          { stCommand = if relative then 0x6D else 0x4D
          , stCurrent = subpath
          , stControl = NoCtrl
          }
  _ -> pure [] -- Not a command: the path ends.
  where
    relative = cmd >= 0x61
    point = do
      Point x y <- coordinatePair
      pure (if relative then Point (cx + x) (cy + y) else Point x y)
    continue p c = walk st {stCurrent = p, stControl = c}
    reflect (Point x y) (Point qx qy) = Point (2 * x - qx) (2 * y - qy)
    ending p = F.withOption p pure (pure [])

--------------------------------------------------------------------------------
-- Transforms
--------------------------------------------------------------------------------

-- | Compose a transform list in SVG order; the rightmost transform acts first.
--
-- Always succeeds. Unknown functions or wrong argument counts contribute
-- identity if their numeric argument list parses. Malformed syntax ends
-- the list, retaining the transforms already read.
transformList :: P Matrix
transformList = skipWspComma *> go identity
  where
    go m = F.withOption transformItem (\m' -> go (m `multiply` m')) (pure m)

-- | Parse a @transform@ attribute using 'transformList'. Returns 'identity'
-- when no usable transform is read.
parseTransform :: ByteString -> Matrix
parseTransform src = maybe identity fst (runPartial src transformList)

transformItem :: P Matrix
transformItem = do
  name <- takeWhileByte isAsciiAlpha
  skipWsp
  skipSatisfyByte (== 0x28)
  args <- numberList
  skipWsp
  skipSatisfyByte (== 0x29)
  skipWspComma
  pure case name of
    "matrix" | [a, b, c, d, e, f] <- args -> Matrix a b c d e f
    "translate" -> case args of
      [x] -> translation x 0
      [x, y] -> translation x y
      _ -> identity
    "scale" -> case args of
      [k] -> scaling k k
      [sx, sy] -> scaling sx sy
      _ -> identity
    "rotate" -> case args of
      [a] -> rotation a
      [a, cx, cy] ->
        translation cx cy `multiply` rotation a `multiply` translation (-cx) (-cy)
      _ -> identity
    "skewX" | [a] <- args -> Matrix 1 0 (tan (radians a)) 1 0 0
    "skewY" | [a] <- args -> Matrix 1 (tan (radians a)) 0 1 0 0
    _ -> identity
  where
    radians a = a * pi / 180

--------------------------------------------------------------------------------
-- The shape elements
--------------------------------------------------------------------------------

-- | @rectSegments x y width height rx ry@ constructs a closed rectangle.
-- Missing corner radii default to each other, or zero if both are absent.
-- Radii are made nonnegative and capped at half the corresponding side.
-- Nonpositive width or height returns @[]@.
rectSegments ::
  Float -> Float -> Float -> Float -> Maybe Float -> Maybe Float -> [Segment]
rectSegments x y w h mrx mry
  | w <= 0 || h <= 0 = []
  | rx <= 0 || ry <= 0 =
      [ MoveTo (Point x y)
      , LineTo (Point (x + w) y)
      , LineTo (Point (x + w) (y + h))
      , LineTo (Point x (y + h))
      , ClosePath
      ]
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
    rx = min (w / 2) (abs (fromMaybe 0 (mrx <|> mry)))
    ry = min (h / 2) (abs (fromMaybe 0 (mry <|> mrx)))
    -- A quarter ellipse, the short way round, clockwise on screen.
    corner = ArcTo rx ry 0 False True

-- | @circleSegments cx cy radius@ constructs a closed circle from two arcs.
-- A nonpositive radius returns @[]@.
circleSegments :: Float -> Float -> Float -> [Segment]
circleSegments cx cy r = ellipseSegments cx cy r r

-- | @ellipseSegments cx cy rx ry@ constructs a closed ellipse from two arcs.
-- Either radius being nonpositive returns @[]@.
ellipseSegments :: Float -> Float -> Float -> Float -> [Segment]
ellipseSegments cx cy rx ry
  | rx <= 0 || ry <= 0 = []
  | otherwise =
      [ MoveTo (Point (cx + rx) cy)
      , ArcTo rx ry 0 False True (Point (cx - rx) cy)
      , ArcTo rx ry 0 False True (Point (cx + rx) cy)
      , ClosePath
      ]

-- | @lineSegments x1 y1 x2 y2@ constructs an open line between two endpoints.
lineSegments :: Float -> Float -> Float -> Float -> [Segment]
lineSegments x1 y1 x2 y2 = [MoveTo (Point x1 y1), LineTo (Point x2 y2)]

-- | Connect points with straight lines. 'True' appends 'ClosePath' for a
-- polygon; 'False' produces an open polyline. An empty input returns @[]@.
polySegments :: Bool -> [Point] -> [Segment]
polySegments closed = \case
  [] -> []
  p : ps -> MoveTo p : map LineTo ps <> [ClosePath | closed]
