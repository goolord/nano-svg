-- | Unit tests for SVG parsing and edge cases, plus properties for number
-- conversion and relative path commands.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import Graphics.NanoSvg
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "nano-svg"
      [ numbers
      , lengths
      , colors
      , paths
      , transforms
      , xml
      , documents
      ]

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

numbers :: TestTree
numbers =
  testGroup
    "numbers"
    [ testCase "the forms the path grammar allows" do
        parseNumber "1" @?= Just 1
        parseNumber "-1" @?= Just (-1)
        parseNumber "+1" @?= Just 1
        parseNumber "1.5" @?= Just 1.5
        parseNumber ".5" @?= Just 0.5
        parseNumber "-.5" @?= Just (-0.5)
        parseNumber "1." @?= Just 1
        parseNumber "1e3" @?= Just 1000
        parseNumber "1E3" @?= Just 1000
        parseNumber "-.5e-3" @?= Just (-0.0005)
        parseNumber "1.5e+2" @?= Just 150
    , testCase "and rejects what is not a number" do
        parseNumber "" @?= Nothing
        parseNumber "." @?= Nothing
        parseNumber "-" @?= Nothing
        parseNumber "e3" @?= Nothing
        parseNumber "1 2" @?= Nothing
        parseNumber "abc" @?= Nothing
    , testCase "an exponent with no digits is not an exponent" do
        -- The @e@ belongs to the unit, so this is one em and not a failure.
        parseLength "1e1px" @?= Just 10
        styleStrokeWidth (shapeStyle (oneShape "<svg><path stroke-width='1em' d='M0 0L1 1'/></svg>")) @?= 1
    , testCase "many digits still land on the right value" do
        parseNumber "0.30000000000000004" @?= Just 0.3
        parseNumber "123456789012345678901234" @?= Just 1.2345679e23
    , testCase "a number a Float cannot hold is not a number" do
        -- Infinity in a coordinate, or the NaN that @0 * Infinity@ makes,
        -- would reach whatever draws the result.
        parseNumber "1e400" @?= Nothing
        parseNumber "-1e400" @?= Nothing
        parseNumber (BC.replicate 400 '9') @?= Nothing
        parseNumber "1e-400" @?= Just 0
    , testCase "but a large exponent on a small mantissa, or the reverse, is" do
        parseNumber "0e400" @?= Just 0
        parseNumber "-0.000e9999" @?= Just 0
        parseNumber ("1" <> BC.replicate 400 '0' <> "e-400") @?= Just 1
        parseNumber ("0." <> BC.replicate 400 '0' <> "15e401") @?= Just 1.5
        parsePath "M1 1L1e400 0" @?= [MoveTo (Point 1 1)]
        sizeOf "<svg width='1e400' height='1e400'/>" @?= (24, 24)
    , testCase "an odd point is dropped" $
        map (toList . shapeSegments) (shapesOf "<svg><polyline points='0,0 1,1 2'/></svg>")
          @?= [[MoveTo (Point 0 0), LineTo (Point 1 1)]]
    , testProperty "agrees with read, to single precision" \(x :: Float) ->
        let rendered = BC.pack (show x)
         in counterexample (show rendered) $
              maybe False (nearly x) (parseNumber rendered)
    ]

-- | Equal to within a single-precision ulp or so, and equal on the
-- exceptional values.
nearly :: Float -> Float -> Bool
nearly a b
  | isNaN a = isNaN b
  | isInfinite a || isInfinite b = a == b
  | otherwise = abs (a - b) <= 1e-5 * max 1 (max (abs a) (abs b))

--------------------------------------------------------------------------------
-- Lengths
--------------------------------------------------------------------------------

lengths :: TestTree
lengths =
  testGroup
    "lengths"
    [ testCase "the absolute ones resolve at 96 dpi" do
        parseLength "10" @?= Just 10
        parseLength "10px" @?= Just 10
        parseLength "10PX" @?= Just 10
        parseLength "10 " @?= Just 10
        parseLength "10 px" @?= Nothing
        parseLength "1in" @?= Just 96
        parseLength "72pt" @?= Just 96
        parseLength "6pc" @?= Just 96
        fmap (round :: Float -> Int) (parseLength "2.54cm") @?= Just 96
        fmap (round :: Float -> Int) (parseLength "25.4mm") @?= Just 96
    , testCase "and the relative ones do not" do
        parseLength "50%" @?= Nothing
        parseLength "2em" @?= Nothing
        parseLength "2ex" @?= Nothing
        parseLength "2foo" @?= Nothing
    ]

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

colors :: TestTree
colors =
  testGroup
    "colors"
    [ testCase "hexadecimal, short and long" do
        parseColor "#f00" @?= Just (rgba 255 0 0 255)
        parseColor "#FF0000" @?= Just (rgba 255 0 0 255)
        parseColor "#f008" @?= Just (rgba 255 0 0 0x88)
        parseColor "#ff000080" @?= Just (rgba 255 0 0 0x80)
        parseColor "#ff00" @?= Just (rgba 255 255 0 0)
        parseColor "#fffff" @?= Nothing
    , testCase "rgb, however it is written" do
        parseColor "rgb(255,0,0)" @?= Just (rgba 255 0 0 255)
        parseColor "rgb(255 0 0)" @?= Just (rgba 255 0 0 255)
        parseColor "rgb( 100%, 0%, 0% )" @?= Just (rgba 255 0 0 255)
        parseColor "rgba(255,0,0,0.5)" @?= Just (rgba 255 0 0 128)
        parseColor "rgb(255 0 0 / 50%)" @?= Just (rgba 255 0 0 128)
        parseColor "rgb(300,-20,0)" @?= Just (rgba 255 0 0 255)
    , testCase "hsl" do
        parseColor "hsl(0,100%,50%)" @?= Just (rgba 255 0 0 255)
        parseColor "hsl(120, 100%, 50%)" @?= Just (rgba 0 255 0 255)
        parseColor "hsl(240deg 100% 50%)" @?= Just (rgba 0 0 255 255)
        parseColor "hsl(0, 0%, 100%)" @?= Just (rgba 255 255 255 255)
        parseColor "hsla(0,100%,50%,0.5)" @?= Just (rgba 255 0 0 128)
    , testCase "all 148 keywords, in any case" do
        parseColor "red" @?= Just (rgba 255 0 0 255)
        parseColor "REBECCAPURPLE" @?= Just (rgba 0x66 0x33 0x99 255)
        parseColor "aliceblue" @?= Just (rgba 0xf0 0xf8 0xff 255)
        parseColor "yellowgreen" @?= Just (rgba 0x9a 0xcd 0x32 255)
        parseColor "lightgoldenrodyellow" @?= Just (rgba 0xfa 0xfa 0xd2 255)
        parseColor "grey" @?= parseColor "gray"
        parseColor "notacolor" @?= Nothing
        parseColor "transparent" @?= Nothing
        show (rgba 1 2 3 255) @?= "#010203ff"
    , testCase "paint keywords are not colors" do
        parsePaint "none" @?= Just PaintNone
        parsePaint "transparent" @?= Just PaintNone
        parsePaint "currentColor" @?= Just PaintCurrent
        parsePaint " currentcolor " @?= Just PaintCurrent
        parsePaint "red" @?= Just (PaintColor (rgba 255 0 0 255))
        parsePaint "rgb(0,0,0)" @?= Just (PaintColor (rgba 0 0 0 255))
    , testCase "and what cannot be read is not a paint" do
        parsePaint "url(#grad)" @?= Nothing
        parsePaint "inherit" @?= Nothing
        parsePaint "" @?= Nothing
    ]

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

paths :: TestTree
paths =
  testGroup
    "path data"
    [ testCase "absolute commands" $
        parsePath "M1 2 L3 4 H5 V6 Z"
          @?= [ MoveTo (Point 1 2)
              , LineTo (Point 3 4)
              , LineTo (Point 5 4)
              , LineTo (Point 5 6)
              , ClosePath
              ]
    , testCase "relative commands are resolved" $
        parsePath "m1 2 l1 1 h1 v1 z"
          @?= [ MoveTo (Point 1 2)
              , LineTo (Point 2 3)
              , LineTo (Point 3 3)
              , LineTo (Point 3 4)
              , ClosePath
              ]
    , testCase "a run of arguments repeats the command" $
        parsePath "M1 1 2 2 3 3"
          @?= [MoveTo (Point 1 1), LineTo (Point 2 2), LineTo (Point 3 3)]
    , testCase "and a relative moveto repeats as a relative lineto" $
        parsePath "m1 1 1 1 1 1"
          @?= [MoveTo (Point 1 1), LineTo (Point 2 2), LineTo (Point 3 3)]
    , testCase "after a closepath the current point is the subpath start" do
        -- An explicit command after the Z draws from where the subpath
        -- began, not from where it ended.
        parsePath "M2 2 L4 4 Z l1 1"
          @?= [ MoveTo (Point 2 2)
              , LineTo (Point 4 4)
              , ClosePath
              , LineTo (Point 3 3)
              ]
        -- A bare pair after it opens a new subpath there instead.
        parsePath "M2 2 L4 4 Z 1 1"
          @?= [ MoveTo (Point 2 2)
              , LineTo (Point 4 4)
              , ClosePath
              , MoveTo (Point 1 1)
              ]
    , testCase "S reflects a cubic control point" $
        parsePath "M0 0 C1 1 2 2 3 3 S5 5 6 6"
          @?= [ CubicTo (Point 1 1) (Point 2 2) (Point 3 3)
              , CubicTo (Point 4 4) (Point 5 5) (Point 6 6)
              ]
            `prefixedByMove` Point 0 0
    , testCase "but not a quadratic one" $
        -- The previous command is a Q, so the S has nothing to reflect and
        -- its first control point is the current point.
        parsePath "M0 0 Q1 1 2 2 S5 5 6 6"
          @?= [ QuadTo (Point 1 1) (Point 2 2)
              , CubicTo (Point 2 2) (Point 5 5) (Point 6 6)
              ]
            `prefixedByMove` Point 0 0
    , testCase "T reflects a quadratic control point" $
        parsePath "M0 0 Q1 1 2 2 T4 4"
          @?= [QuadTo (Point 1 1) (Point 2 2), QuadTo (Point 3 3) (Point 4 4)]
            `prefixedByMove` Point 0 0
    , testCase "arc flags need no separator" $
        parsePath "M2 10a8 8 0 1 1 16 0z"
          @?= parsePath "M2 10a8 8 0 1116 0z"
    , testCase "and run into the numbers after them" $
        parsePath "M0 0a1 1 0 00.5.5"
          @?= [MoveTo (Point 0 0), ArcTo 1 1 0 False False (Point 0.5 0.5)]
    , testCase "a malformed tail keeps what came before it" do
        parsePath "M1 1 L2 2 L3" @?= [MoveTo (Point 1 1), LineTo (Point 2 2)]
        parsePath "M1 1 L2 2 X9 9" @?= [MoveTo (Point 1 1), LineTo (Point 2 2)]
        parsePath "" @?= []
        parsePath "nonsense" @?= []
    , testCase "commas and exponents in a d" $
        parsePath "M1e1,1e1L2e1,2e1"
          @?= [MoveTo (Point 10 10), LineTo (Point 20 20)]
    , testProperty "a relative path draws where the absolute one does" \ps ->
        let steps = take 12 (map (\(Small a, Small b) -> (a, b)) ps) :: [(Int, Int)]
            absolute = scanl1 (\(x, y) (dx, dy) -> (x + dx, y + dy)) steps
            render c = BC.unwords [BC.pack (show a <> " " <> show b) | (a, b) <- c]
         in not (null steps) ==>
              parsePath ("M0 0 l" <> render steps)
                == parsePath ("M0 0 L" <> render absolute)
    ]

-- | A path's segments after the moveto that opened it.
prefixedByMove :: [Segment] -> Point -> [Segment]
prefixedByMove segs p = MoveTo p : segs

--------------------------------------------------------------------------------
-- Transforms
--------------------------------------------------------------------------------

transforms :: TestTree
transforms =
  testGroup
    "transforms"
    [ testCase "each function" do
        parseTransform "matrix(1 2 3 4 5 6)" @?= Matrix 1 2 3 4 5 6
        parseTransform "translate(5)" @?= Matrix 1 0 0 1 5 0
        parseTransform "translate(5, 6)" @?= Matrix 1 0 0 1 5 6
        parseTransform "scale(2)" @?= Matrix 2 0 0 2 0 0
        parseTransform "scale(2,3)" @?= Matrix 2 0 0 3 0 0
        parseTransform "" @?= identity
        parseTransform "nonsense" @?= identity
    , testCase "a rotation about a point leaves that point alone" $
        let m = parseTransform "rotate(90 10 10)"
            Point x y = transformPoint m (Point 10 10)
         in (round x, round y) @?= (10 :: Int, 10 :: Int)
    , testCase "a list composes left to right" $
        let m = parseTransform "translate(10 0) scale(2)"
         in transformPoint m (Point 1 1) @?= Point 12 2
    , testCase "and the outer one is applied last" $
        let m = parseTransform "scale(2) translate(10 0)"
         in transformPoint m (Point 1 1) @?= Point 22 2
    , testCase "a scale shows up in the length factor" do
        averageScale (parseTransform "scale(3)") @?= 3
        averageScale (parseTransform "translate(9 9)") @?= 1
    ]

--------------------------------------------------------------------------------
-- XML
--------------------------------------------------------------------------------

xml :: TestTree
xml =
  testGroup
    "xml"
    [ testCase "namespace prefixes are dropped" do
        length (shapesOf "<svg:svg><svg:rect width='1' height='1'/></svg:svg>") @?= 1
        length (shapesOf useDoc) @?= 1
    , testCase "comments, instructions, doctypes and byte-order marks are skipped" do
        sizeOf "<?xml version='1.0'?><!--hi--><svg width='3'/>" @?= (3, 24)
        sizeOf "<!DOCTYPE svg PUBLIC \"x\" \"y\"><svg width='3'/>" @?= (3, 24)
        sizeOf "<!DOCTYPE svg [<!ENTITY x \"y\">]><svg width='3'/>" @?= (3, 24)
        sizeOf "ï»¿<svg width='3'/>" @?= (3, 24)
    , testCase "entities in attribute values" do
        styleFill (shapeStyle (oneShape "<svg><path fill='&#x72;&#101;&#X64;' d='M0 0L1 1'/></svg>"))
          @?= Just (PaintColor (rgba 255 0 0 255))
        map (length . toList . shapeSegments) (shapesOf "<svg><path d='M0 0&#10;L1 1'/></svg>") @?= [2]
    , testCase "an ampersand that begins nothing stands for itself" $
        map (length . toList . shapeSegments) (shapesOf "<svg><path d='M0 0 &amp L1 1 &nosuch; &#;'/></svg>")
          @?= [1]
    , testCase "and so does a reference past the last code point" $
        -- Would wrap around to 'r' in 64-bit arithmetic.
        styleFill (shapeStyle (oneShape "<svg><path fill='&#x10000000000000072;ed' d='M0 0L1 1'/></svg>"))
          @?= Nothing
    , testCase "a document that is not one" $
        assertBool "expected a Left" (isLeft (parseSvg "<svg><b></svg>"))
    ]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

--------------------------------------------------------------------------------
-- Documents
--------------------------------------------------------------------------------

documents :: TestTree
documents =
  testGroup
    "documents"
    [ testCase "a document needs an svg element" do
        assertBool "expected a Left" (isLeft (parseSvg "<nope/>"))
        assertBool "expected a Left" (isLeft (parseSvg "not xml at all"))
    , testCase "the size and the box" do
        sizeOf "<svg width='48' height='32'/>" @?= (48, 32)
        sizeOf "<svg viewBox='0 0 24 24'/>" @?= (24, 24)
        sizeOf "<svg width='48' height='48' viewBox='0 0 24 24'/>" @?= (48, 48)
        sizeOf "<svg width='1in' height='1in'/>" @?= (96, 96)
        -- A percentage is not a length, so the viewBox decides.
        sizeOf "<svg width='100%' height='100%' viewBox='0 0 16 16'/>" @?= (16, 16)
        sizeOf "<svg/>" @?= (24, 24)
        boxOf "<svg viewBox='1 2 24 25'/>" @?= Box 1 2 24 25
        -- A viewBox that is not four numbers, or has no extent, is not one.
        boxOf "<svg width='8' height='9' viewBox='0 0 24'/>" @?= Box 0 0 8 9
        boxOf "<svg width='8' height='9' viewBox='0 0 24 24 24'/>" @?= Box 0 0 8 9
        boxOf "<svg width='8' height='9' viewBox='0 0 0 24'/>" @?= Box 0 0 8 9
    , testCase "shapes come out in paint order" $
        map (length . toList . shapeSegments) (shapesOf clock) @?= [4, 3]
    , testCase "an icon drawn in currentColor is monochrome" do
        fmap documentMonochrome (parseSvg clock) @?= Right True
        fmap documentMonochrome (parseSvg holed) @?= Right False
        fmap documentMonochrome (parseSvg "<svg><rect width='1' height='1'/></svg>")
          @?= Right True
    , testCase "presentation attributes are inherited" $
        let sh =
              oneShape "<svg stroke-width='3'><g stroke='red'><path d='M0 0L1 1'/></g></svg>"
         in do
              styleStrokeWidth (shapeStyle sh) @?= 3
              styleStroke (shapeStyle sh) @?= Just (PaintColor (rgba 255 0 0 255))
    , testCase "a style attribute outranks them" $
        let sh = oneShape "<svg><path fill='red' style='fill: blue' d='M0 0L1 1'/></svg>"
         in styleFill (shapeStyle sh) @?= Just (PaintColor (rgba 0 0 255 255))
    , testCase "a value that cannot be read leaves the inherited one" $
        let sh = oneShape "<svg fill='red'><path fill='url(#grad)' d='M0 0L1 1'/></svg>"
         in styleFill (shapeStyle sh) @?= Just (PaintColor (rgba 255 0 0 255))
    , testCase "an opacity may be a percentage" do
        styleOpacity (shapeStyle (oneShape "<svg><path opacity='50%' d='M0 0L1 1'/></svg>"))
          @?= 0.5
        styleFillOpacity
          (shapeStyle (oneShape "<svg><path fill-opacity='25%' d='M0 0L1 1'/></svg>"))
          @?= 0.25
    , testCase "opacity multiplies down the tree" $
        let sh = oneShape "<svg opacity='0.5'><g opacity='0.5'><path d='M0 0L1 1'/></g></svg>"
         in styleOpacity (shapeStyle sh) @?= 0.25
    , testCase "and a style attribute outranks a presentation one there too" $
        styleOpacity
          ( shapeStyle
              (oneShape "<svg><path opacity='0.5' style='opacity:0.25' d='M0 0L1 1'/></svg>")
          )
          @?= 0.25
    , testCase "a switch draws the first child that draws anything" do
        map (length . toList . shapeSegments) (shapesOf switchDoc) @?= [2]
        shapesOf "<svg><switch><desc>nothing here</desc></switch></svg>" @?= []
    , testCase "transforms compose down it" $
        let sh =
              oneShape
                "<svg><g transform='translate(5 0)'><g transform='scale(2)'><path d='M1 1L1 1'/></g></g></svg>"
         in transformPoint (shapeTransform sh) (Point 1 1) @?= Point 7 2
    , testCase "the shape elements each become segments" do
        map (length . toList . shapeSegments) (shapesOf rectangles) @?= [5, 10]
        map
          (length . toList . shapeSegments)
          (shapesOf "<svg><circle r='5'/><ellipse rx='5' ry='3'/></svg>")
          @?= [4, 4]
        map
          (length . toList . shapeSegments)
          (shapesOf "<svg><line x2='5' y2='5'/></svg>")
          @?= [2]
        -- A polyline is not closed, so two points are two segments; the
        -- polygon's three are a moveto, two linetos and a closepath.
        map (length . toList . shapeSegments) (shapesOf polys) @?= [2, 4]
    , testCase "a shape with no extent draws nothing" do
        shapesOf "<svg><rect width='0' height='5'/></svg>" @?= []
        shapesOf "<svg><circle r='0'/></svg>" @?= []
        shapesOf "<svg><path d=''/></svg>" @?= []
    , testCase "defs are not drawn where they stand" $
        shapesOf "<svg><defs><rect id='r' width='1' height='1'/></defs></svg>" @?= []
    , testCase "but a use draws them, offset and styled" $
        let sh = oneShape useDoc
         in do
              transformPoint (shapeTransform sh) (Point 0 0) @?= Point 5 6
              styleFill (shapeStyle sh) @?= Just (PaintColor (rgba 255 0 0 255))
    , testCase "a use of a symbol draws its children" $
        length (shapesOf symbolDoc) @?= 2
    , testCase "the same target may be used more than once" $
        length (shapesOf twiceDoc) @?= 2
    , testCase "a duplicated id refers to its first element" $
        map (length . toList . shapeSegments)
          (shapesOf "<svg><defs><line id='d' x2='1'/><rect id='d' width='1' height='1'/></defs><use href='#d'/></svg>")
          @?= [2]
    , testCase "and a circular one draws each target once" do
        -- A cycle is cut where it closes, so a group that references
        -- itself several times cannot fan out: counting depth alone would
        -- let four self-references become tens of thousands of shapes.
        length (shapesOf (circularFanout 1)) @?= 1
        length (shapesOf (circularFanout 4)) @?= 1
        length (shapesOf (circularFanout 12)) @?= 1
    , testCase "and a self-referential one terminates" $
        length (shapesOf circular) <= 8 @?= True
    , testCase "display and visibility prune the subtree" do
        shapesOf "<svg><g display='none'><rect width='1' height='1'/></g></svg>" @?= []
        shapesOf "<svg><rect style='display:none' width='1' height='1'/></svg>" @?= []
        shapesOf "<svg><g visibility='hidden'><rect width='1' height='1'/></g></svg>"
          @?= []
        length
          (shapesOf "<svg><g visibility='visible'><rect width='1' height='1'/></g></svg>")
          @?= 1
    , testCase "and a style declaration outranks the attribute there too" do
        length
          ( shapesOf
              "<svg><rect display='none' style='display:inline' width='1' height='1'/></svg>"
          )
          @?= 1
        shapesOf
          "<svg><rect display='inline' style='display:none' width='1' height='1'/></svg>"
          @?= []
    , testCase "text, gradients and the rest are skipped" $
        shapesOf
          "<svg><text x='0'>hi</text><linearGradient id='g'/><foreignObject/></svg>"
          @?= []
    , testCase "the key follows the source" do
        fmap documentKey (parseSvg clock) @?= fmap documentKey (parseSvg clock)
        assertBool "different sources, different keys" $
          fmap documentKey (parseSvg clock) /= fmap documentKey (parseSvg holed)
    , testCase "and two documents are equal when their sources are" do
        parseSvg clock @?= parseSvg clock
        assertBool "different sources, different documents" $
          parseSvg clock /= parseSvg holed
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | A Lucide-style icon: the shape the overwhelming majority of icon sets
-- take, down to the comment in the middle.
clock :: ByteString
clock =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" \
  \stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">\
  \<!-- a clock --><circle cx=\"12\" cy=\"12\" r=\"10\"/><path d=\"M12 6v6l4 2\"/></svg>"

holed :: ByteString
holed =
  "<svg viewBox='0 0 10 10'><path fill-rule='evenodd' fill='#ff0000' d='M0 0h10v10H0z M3 3h4v4H3z'/>\
  \<g transform='translate(5 0) scale(0.5)'><rect width='2' height='2' fill='rgb(0,0,255)'/></g></svg>"

rectangles :: ByteString
rectangles =
  "<svg><rect width='4' height='4'/><rect width='4' height='4' rx='1'/></svg>"

polys :: ByteString
polys =
  "<svg><polyline points='0,0 1,1'/><polygon points='0,0 1,0 1,1'/></svg>"

useDoc :: ByteString
useDoc =
  "<svg><defs><rect id='r' width='1' height='1'/></defs>\
  \<use xlink:href='#r' x='5' y='6' fill='red'/></svg>"

symbolDoc :: ByteString
symbolDoc =
  "<svg><symbol id='s'><rect width='1' height='1'/><circle r='1'/></symbol>\
  \<use href='#s'/></svg>"

twiceDoc :: ByteString
twiceDoc =
  "<svg><defs><rect id='r' width='1' height='1'/></defs>\
  \<use href='#r'/><use href='#r' x='5'/></svg>"

-- | A group that refers to itself @n@ times and also draws one rectangle.
circularFanout :: Int -> ByteString
circularFanout n =
  "<svg><defs><g id='a'>"
    <> mconcat (replicate n "<use href='#a'/>")
    <> "<rect width='1' height='1'/></g></defs><use href='#a'/></svg>"

circular :: ByteString
circular =
  "<svg><defs><g id='a'><use href='#b'/></g><g id='b'><use href='#a'/>\
  \<rect width='1' height='1'/></g></defs><use href='#a'/></svg>"

switchDoc :: ByteString
switchDoc =
  "<svg><switch><desc>a description is not a drawing</desc>\
  \<line x2='1' y2='1'/><rect width='9' height='9'/></switch></svg>"

sizeOf :: ByteString -> (Float, Float)
sizeOf = either (error "parse failed") documentSize . parseSvg

boxOf :: ByteString -> Box
boxOf = either (error "parse failed") documentViewBox . parseSvg

shapesOf :: ByteString -> [Shape]
shapesOf = either (error "parse failed") (toList . documentShapes) . parseSvg

-- | The one shape a document was meant to have.
oneShape :: ByteString -> Shape
oneShape src = case shapesOf src of
  [sh] -> sh
  shs -> error ("expected one shape, got " <> show (length shs))
