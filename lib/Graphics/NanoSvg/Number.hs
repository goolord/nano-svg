{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.NanoSvg.Number
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Parse SVG numbers, lengths and coordinate lists.
--
-- Numbers accept an optional sign, decimal point and exponent, including
-- @.5@, @1.@, @1e3@ and @-.5E-3@. Nonfinite results are rejected.
--
-- Absolute lengths convert at 96 user units per inch. Relative units
-- (@%@, @em@, @ex@) are parsed but 'toUserUnits' returns 'Nothing' for them.
module Graphics.NanoSvg.Number
  ( -- * Numbers
    number
  , parseNumber

    -- * Lengths
  , Unit (..)
  , Length (..)
  , length_
  , parseLength
  , toUserUnits
  , parseUserUnits

    -- * Lists
  , numberList
  , parseNumberList
  , coordinatePair
  , pointList
  , parsePointList

    -- * Flags
  , flag
  )
where

import Data.ByteString (ByteString)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromList)
import FlatParse.Basic qualified as F
import GHC.Float (double2Float)
import Graphics.NanoSvg.Internal.Parser
import Graphics.NanoSvg.Types (Point (..))

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

-- | Parse one finite SVG number without leading whitespace.
-- An incomplete exponent is left unconsumed; underflow may produce zero.
number :: P Float
number = do
  negative <- sign
  -- The integer part, then the fraction. Either may be empty, but not both.
  whole <- digits False (Acc 0 0 0)
  Acc m n e <- (dot *> digits True whole) F.<|> pure whole
  if n == 0
    then F.failed
    else do
      -- An @e@ with no digits after it belongs to whatever follows, as in
      -- the @1ex@ of a length, so the exponent backtracks as a whole.
      e' <- (exponentMark *> exponentValue) F.<|> pure 0
      let !x = assemble negative m (e + e')
      -- Reject overflow and NaN, including 0e400 (zero times infinity).
      if isNaN x || isInfinite x then F.failed else pure x
  where
    dot = skipSatisfyByte (== 0x2E)
    exponentMark = skipSatisfyByte (\w -> w == 0x65 || w == 0x45)

-- | Mantissa, digit count and decimal exponent.
data Acc = Acc {-# UNPACK #-} !Word {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Accumulate digits up to 'mantissaLimit'. Further integer digits increase
-- the exponent; further fractional digits are discarded.
{-# INLINE digits #-}
digits :: Bool -> Acc -> P Acc
digits !fractional = go
  where
    go acc@(Acc m n e) =
      F.withOption
        (satisfyByte isDigitByte)
        ( \w ->
            let !d = fromIntegral (w - 0x30)
             in go
                  if m < mantissaLimit
                    then Acc (m * 10 + d) (n + 1) (if fractional then e - 1 else e)
                    else Acc m (n + 1) (if fractional then e else e + 1)
        )
        (pure acc)

-- | Mantissa accumulation limit, retaining more precision than 'Float' needs.
mantissaLimit :: Word
mantissaLimit = 100000000000000000

-- | A leading @-@ or @+@, and whether it was a minus.
sign :: P Bool
sign =
  F.withOption
    (satisfyByte (\w -> w == 0x2D || w == 0x2B))
    (\w -> pure (w == 0x2D))
    (pure False)
{-# INLINE sign #-}

-- | The digits of an exponent, with their sign. An @e@ with nothing usable
-- after it is not an exponent, and the number ends before it.
exponentValue :: P Int
exponentValue = do
  negative <- sign
  Acc m n _ <- digits False (Acc 0 0 0)
  if n == 0
    then F.failed
    else pure $! (if negative then negate else id) (fromIntegral (min 10000 m))

-- | Scale in 'Double', then narrow to 'Float'. 'number' rejects nonfinite
-- results from either step.
assemble :: Bool -> Word -> Int -> Float
assemble negative m e = double2Float (if negative then negate d else d)
  where
    d = fromIntegral m * powerOfTen e :: Double

powerOfTen :: Int -> Double
powerOfTen e
  | e >= 0, e <= 22 = indexPrimArray tenTable e
  | e < 0, e >= -22 = 1 / indexPrimArray tenTable (negate e)
  | otherwise = 10 ** fromIntegral e

-- | @10 ^ k@ for @k@ up to 22, the largest power of ten a 'Double' holds
-- exactly.
tenTable :: PrimArray Double
tenTable = primArrayFromList [10 ^ k | k <- [0 :: Int .. 22]]
{-# NOINLINE tenTable #-}

-- | Parse one complete number with optional surrounding whitespace.
-- Returns 'Nothing' for invalid input, trailing content or nonfinite results.
parseNumber :: ByteString -> Maybe Float
parseNumber src = evaluate src (skipWsp *> number <* skipWsp)

--------------------------------------------------------------------------------
-- Lengths
--------------------------------------------------------------------------------

-- | The unit a length was written in.
data Unit
  = -- | No unit: already user units.
    UserSpace
  | -- | @px@, which SVG defines as one user unit.
    Px
  | -- | @pt@: 1\/72 inch.
    Pt
  | -- | @pc@: 1\/6 inch.
    Pc
  | -- | @in@.
    Inch
  | -- | @cm@.
    Cm
  | -- | @mm@.
    Mm
  | -- | @em@: relative to the font size.
    Em
  | -- | @ex@: relative to the x-height.
    Ex
  | -- | @%@: relative to the viewport.
    Percent
  deriving (Eq, Show)

-- | A number and its unit.
data Length = Length {-# UNPACK #-} !Float !Unit
  deriving (Eq, Show)

-- | Parse a number and optional unit without leading whitespace.
-- Unit suffixes are case-insensitive and must immediately follow the number.
length_ :: P Length
length_ = Length <$> number <*> (unit F.<|> pure UserSpace)
  where
    unit =
      F.withAnyWord8 \a -> case a of
        0x25 -> pure Percent
        _ ->
          F.withAnyWord8 \b -> case (lower a, lower b) of
            (0x70, 0x78) -> pure Px -- px
            (0x70, 0x74) -> pure Pt -- pt
            (0x70, 0x63) -> pure Pc -- pc
            (0x69, 0x6E) -> pure Inch -- in
            (0x63, 0x6D) -> pure Cm -- cm
            (0x6D, 0x6D) -> pure Mm -- mm
            (0x65, 0x6D) -> pure Em -- em
            (0x65, 0x78) -> pure Ex -- ex
            _ -> F.failed

-- | Parse one complete length with optional surrounding whitespace.
-- Unknown units or trailing content return 'Nothing'.
parseLength :: ByteString -> Maybe Length
parseLength src = evaluate src (skipWsp *> length_ <* skipWsp)

-- | Convert at 96 user units per inch. Returns 'Nothing' for 'Em', 'Ex' and
-- 'Percent', which require font or viewport context.
toUserUnits :: Length -> Maybe Float
toUserUnits (Length x u) = case u of
  UserSpace -> Just x
  Px -> Just x
  Pt -> Just (x * 96 / 72)
  Pc -> Just (x * 16)
  Inch -> Just (x * 96)
  Cm -> Just (x * 96 / 2.54)
  Mm -> Just (x * 96 / 25.4)
  Em -> Nothing
  Ex -> Nothing
  Percent -> Nothing

-- | 'parseLength' followed by 'toUserUnits'.
parseUserUnits :: ByteString -> Maybe Float
parseUserUnits src = parseLength src >>= toUserUnits

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | Numbers separated by whitespace and commas, stopping at the first thing
-- that is not a number.
numberList :: P [Float]
numberList = skipWspComma *> go
  where
    go = F.withOption number (\x -> (x :) <$> (skipWspComma *> go)) (pure [])

-- | Parse the initial number list, ignoring any malformed tail.
-- Returns @[]@ if no number can be read.
parseNumberList :: ByteString -> [Float]
parseNumberList src = maybe [] fst (runPartial src numberList)

-- | Two numbers, which may be separated by a comma, whitespace, or in the
-- case of @1-2@ or @1.5.5@ by nothing at all.
coordinatePair :: P Point
coordinatePair = do
  x <- number
  skipWspComma
  y <- number
  pure (Point x y)
{-# INLINE coordinatePair #-}

-- | Parse coordinate pairs separated by whitespace or commas.
-- Stops before an incomplete pair or malformed tail.
pointList :: P [Point]
pointList = skipWspComma *> go
  where
    go = F.withOption coordinatePair (\p -> (p :) <$> (skipWspComma *> go)) (pure [])

-- | Parse a @points@ attribute. Keeps complete pairs before a malformed tail
-- and drops an unmatched final coordinate.
parsePointList :: ByteString -> [Point]
parsePointList src = maybe [] fst (runPartial src pointList)

--------------------------------------------------------------------------------
-- Flags
--------------------------------------------------------------------------------

-- | An arc flag: a single @0@ or @1@, which may run straight into the
-- number after it, as in @a1 1 0 00.5.5@.
flag :: P Bool
flag = satisfyByte (\w -> w == 0x30 || w == 0x31) >>= \w -> pure (w == 0x31)
{-# INLINE flag #-}
