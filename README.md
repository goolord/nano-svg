# nano-svg

A fast SVG parser for Haskell. Bytes in, a flat array of shapes out.

For geometry renderers, rasterizers and UI toolkits that need SVG icons and
static vector artwork as paths. The library parses and normalizes geometry;
the caller handles rendering and viewport mapping.

## Quick start

`parseSvg :: ByteString -> Either String Document` reads UTF-8 SVG directly
from a strict `ByteString`—no text-decoding step needed.

```haskell
import Graphics.NanoSvg
import qualified Data.ByteString as BS

main :: IO ()
main = do
  bytes <- BS.readFile "clock.svg"
  case parseSvg bytes of
    Left err -> putStrLn err
    Right doc -> print (documentSize doc, length (documentShapes doc))
```

A `Document` contains the viewport size, the `viewBox` and a `SmallArray` of
`Shape`s in paint order. Each shape carries its path, the `Matrix` that maps
it into the `viewBox`, and its resolved `Style`. Relative path commands,
shorthands and basic shapes become a common set of absolute path segments.
The renderer walks segments without resolving an SVG tree, style inheritance
or a transform stack. It still tracks the current point and subpath start.

`encodeSvg :: Document -> ByteString` writes a document back out as SVG, one
`path` per shape; `svgBuilder` gives the same as a bytestring `Builder`.
Parsing the result gives back the same shapes.

## SVG scope

Supports paths, rectangles, circles, ellipses, lines, polylines and polygons;
groups and `use` references; fills, strokes and inline styles; and SVG
transforms. The focus is the static geometry used in icons and vector assets.

Not supported: gradients, patterns and other paint servers; `text`;
`clipPath`, `mask` and `filter`; `marker`; CSS in a `style` element or an
external sheet, and so `class` selectors; animation; and
`preserveAspectRatio`, which is left to the renderer.

Unknown elements are skipped along with their children. Group opacity is
multiplied into each shape rather than preserved as a compositing group.
Nested `svg` and referenced `symbol` elements do not establish viewports.

## Modules

| Module | Purpose |
| --- | --- |
| `Graphics.NanoSvg` | `parseSvg` and `encodeSvg`, re-exporting the two below |
| `Graphics.NanoSvg.Types` | the document model and matrix helpers |
| `Graphics.NanoSvg.Attribute` | standalone parsers for paths, transforms, colors, numbers and lengths |

## Build

```
cabal build
cabal test
cabal haddock --open
```
