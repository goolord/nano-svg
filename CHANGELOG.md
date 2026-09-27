# Revision history for nano-svg

## 0.2.0.0

* Breaking: collapse into three modules. `Graphics.NanoSvg.Color`, `.Number`,
  `.Path`, `.Xml` and `.Internal.Parser` are gone; their value parsers live in
  `Graphics.NanoSvg.Attribute`.
* Breaking: remove `documentWidth`/`documentHeight`, the `RGBA` channel
  accessors, `withAlpha`, `transparent`, `Length`/`Unit` and the raw flatparse
  parsers. `parseLength` now returns user units.
* Add `encodeSvg` and `svgBuilder`, which write a `Document` back out as SVG
  with bytestring builders. `parseSvg` reads the result back to the same
  shapes.
* `use` elements nest at most `maxUseDepth` (8) deep, so a small file cannot
  expand exponentially.
* Entity decoding is linear in the attribute length, and numeric character
  references no longer build an `Integer` from any number of digits.
* Look up `use` targets and named colors in maps, and share a target's
  geometry across its uses.
* `0e400` and long mantissas parse as finite numbers.

## 0.1.0.0

* First version.
