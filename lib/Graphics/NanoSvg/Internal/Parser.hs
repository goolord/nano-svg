-- |
-- Module      : Graphics.NanoSvg.Internal.Parser
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Shared @flatparse@ type, byte combinators and attribute parser runners.
--
-- Parsers carry no error messages. Callers choose whole-input validation
-- with 'evaluate' or prefix parsing with 'runPartial'.
--
-- Exposed because public parser signatures use 'P'; this module's API is
-- not stable.
module Graphics.NanoSvg.Internal.Parser
  ( -- * Parsers
    P

    -- * Running
  , evaluate
  , runPartial

    -- * Bytes
  , satisfyByte
  , skipSatisfyByte
  , skipWhileByte
  , takeWhileByte
  , peekByte

    -- * Character classes
  , isWsp
  , isDigitByte
  , isHexDigit
  , isAsciiAlpha
  , hexValue
  , lower
  , lowercase

    -- * Whitespace
  , skipWsp
  , skipWspComma
  , strip

    -- * Numbers
  , clamp01
  )
where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import FlatParse.Basic qualified as F

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Pure byte parser without diagnostic errors.
type P = F.Parser ()

--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------

-- | Run a parser, requiring complete input consumption. Returns 'Nothing'
-- on failure or leftover input. Does not skip whitespace automatically.
evaluate :: ByteString -> P a -> Maybe a
evaluate src p = case F.runParser (p <* F.eof) src of
  F.OK a _ -> Just a
  _ -> Nothing
{-# INLINE evaluate #-}

-- | Run a parser and return its value and unconsumed input.
-- Returns 'Nothing' on failure.
runPartial :: ByteString -> P a -> Maybe (a, ByteString)
runPartial src p = case F.runParser p src of
  F.OK a rest -> Just (a, rest)
  _ -> Nothing
{-# INLINE runPartial #-}

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

-- | One byte matching a predicate.
satisfyByte :: (Word8 -> Bool) -> P Word8
satisfyByte f = F.withAnyWord8 \w -> if f w then pure w else F.failed
{-# INLINE satisfyByte #-}

-- | One byte matching a predicate, discarded.
skipSatisfyByte :: (Word8 -> Bool) -> P ()
skipSatisfyByte f = F.withAnyWord8 \w -> if f w then pure () else F.failed
{-# INLINE skipSatisfyByte #-}

-- | Skip zero or more consecutive bytes matching a predicate.
skipWhileByte :: (Word8 -> Bool) -> P ()
skipWhileByte f = F.skipMany (skipSatisfyByte f)
{-# INLINE skipWhileByte #-}

-- | Read zero or more matching bytes as a slice sharing the input buffer.
takeWhileByte :: (Word8 -> Bool) -> P ByteString
takeWhileByte f = F.byteStringOf (skipWhileByte f)
{-# INLINE takeWhileByte #-}

-- | The next byte without consuming it, or 'Nothing' at the end of input.
peekByte :: P (Maybe Word8)
peekByte = F.lookahead (F.optional F.anyWord8)
{-# INLINE peekByte #-}

--------------------------------------------------------------------------------
-- Character classes
--------------------------------------------------------------------------------

-- | Accepted whitespace: space, tab, line feed, carriage return or form feed.
{-# INLINE isWsp #-}
isWsp :: Word8 -> Bool
isWsp w = w == 0x20 || w == 0x09 || w == 0x0A || w == 0x0D || w == 0x0C

-- | An ASCII digit.
{-# INLINE isDigitByte #-}
isDigitByte :: Word8 -> Bool
isDigitByte w = w >= 0x30 && w <= 0x39

-- | A hexadecimal digit, in either case.
{-# INLINE isHexDigit #-}
isHexDigit :: Word8 -> Bool
isHexDigit w =
  isDigitByte w || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)

-- | An ASCII letter.
{-# INLINE isAsciiAlpha #-}
isAsciiAlpha :: Word8 -> Bool
isAsciiAlpha w = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A)

-- | Numeric value of a hex digit. Requires 'isHexDigit'; other inputs give
-- unspecified results.
{-# INLINE hexValue #-}
hexValue :: Word8 -> Word8
hexValue w
  | isDigitByte w = w - 0x30
  | otherwise = lower w - 0x61 + 10

-- | Lowercase an ASCII letter; leave other bytes unchanged.
{-# INLINE lower #-}
lower :: Word8 -> Word8
lower w = if w >= 0x41 && w <= 0x5A then w .|. 0x20 else w

-- | ASCII lowercase. Returns the original buffer if no uppercase letters
-- occur; otherwise copies it. Intended for case-insensitive names.
lowercase :: ByteString -> ByteString
lowercase s
  | BS.all (\w -> w < 0x41 || w > 0x5A) s = s
  | otherwise = BS.map lower s

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

-- | Skip whitespace.
skipWsp :: P ()
skipWsp = skipWhileByte isWsp
{-# INLINE skipWsp #-}

-- | Skip any sequence of whitespace and commas, including repeated commas.
skipWspComma :: P ()
skipWspComma = skipWhileByte \w -> isWsp w || w == 0x2C
{-# INLINE skipWspComma #-}

-- | Drop the whitespace either side of a value.
strip :: ByteString -> ByteString
strip = BS.dropWhile isWsp . BS.dropWhileEnd isWsp

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

-- | Clamp to [0, 1].
{-# INLINE clamp01 #-}
clamp01 :: Float -> Float
clamp01 = max 0 . min 1
