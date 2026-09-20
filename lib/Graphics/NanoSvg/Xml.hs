{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.NanoSvg.Xml
-- Copyright   : (c) 2026 goolord
-- License     : MIT
--
-- Minimal XML element tree backed by @hexml@.
--
-- Names and unmodified attribute values share the parsed input buffer.
-- SVG-specific preprocessing:
--
-- * Replace a DOCTYPE and leading UTF-8 byte-order mark with spaces before
--   parsing. This preserves byte offsets, but not line numbers within a
--   multiline DOCTYPE. Custom entities are not expanded.
--
-- * Strip namespace prefixes without resolving namespace URIs: @svg:path@
--   becomes @path@ and @xlink:href@ becomes @href@.
--
-- * Decode the five predefined XML entities and decimal or hexadecimal
--   character references in attribute values.
--
-- Text nodes, comments and processing instructions are omitted.
module Graphics.NanoSvg.Xml
  ( -- * The tree
    Element (..)
  , Attribute (..)

    -- * Parsing
  , parseXml

    -- * Looking things up
  , attribute
  , descendants

    -- * Pieces
  , localName
  , decodeEntities
  , withoutDoctype
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr)
import Data.Maybe (listToMaybe)
import Graphics.NanoSvg.Internal.Parser (hexValue, isDigitByte, isHexDigit)
import Text.XML.Hexml qualified as Hexml

--------------------------------------------------------------------------------
-- The tree
--------------------------------------------------------------------------------

-- | An element with namespace prefixes removed from its name and attributes.
data Element = Element
  { elementName :: !ByteString
  , elementAttributes :: ![Attribute]
  , elementChildren :: ![Element]
  }
  deriving (Eq, Show)

-- | An attribute with a local name and entity-decoded UTF-8 value.
data Attribute = Attribute
  { attributeName :: !ByteString
  , attributeValue :: !ByteString
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse top-level elements or return a @hexml@ error. Does not require an
-- @svg@ root. The returned tree may retain the parsed input buffer.
parseXml :: ByteString -> Either String [Element]
parseXml src = case Hexml.parse (withoutDoctype (withoutBom src)) of
  Left err -> Left (BC.unpack err)
  Right root -> Right (map element (elements root))
  where
    -- hexml exposes processing instructions as elements; filter them out.
    elements node =
      [n | n <- Hexml.children node, not (BS.isPrefixOf "<?" (Hexml.outer n))]
    element node =
      Element
        { elementName = localName (Hexml.name node)
        , elementAttributes =
            [ Attribute
                { attributeName = localName (Hexml.attributeName a)
                , attributeValue = decodeEntities (Hexml.attributeValue a)
                }
            | a <- Hexml.attributes node
            ]
        , elementChildren = map element (elements node)
        }

--------------------------------------------------------------------------------
-- Looking things up
--------------------------------------------------------------------------------

-- | Find the first attribute with the given local name (case-sensitive).
-- On a parsed tree, @"href"@ also matches an original @xlink:href@.
attribute :: ByteString -> Element -> Maybe ByteString
attribute k el = listToMaybe [v | Attribute n v <- elementAttributes el, n == k]
{-# INLINE attribute #-}

-- | Every element in the subtree, the root itself first, in document order.
descendants :: Element -> [Element]
descendants el = el : concatMap descendants (elementChildren el)

--------------------------------------------------------------------------------
-- Pieces
--------------------------------------------------------------------------------

-- | Everything after the last colon: @svg:path@ becomes @path@.
localName :: ByteString -> ByteString
localName n = case BS.elemIndexEnd 0x3A n of
  Just i -> BS.drop (i + 1) n
  Nothing -> n

-- | The document with a leading UTF-8 byte-order mark replaced by spaces.
withoutBom :: ByteString -> ByteString
withoutBom src
  | BS.isPrefixOf "\xEF\xBB\xBF" src = "   " <> BS.drop 3 src
  | otherwise = src

-- | Replace the first DOCTYPE, including its internal subset, with spaces.
-- Preserves byte offsets, not line numbers. The scanner counts brackets;
-- it does not parse quoted strings or comments inside the declaration.
withoutDoctype :: ByteString -> ByteString
withoutDoctype bytes = case BS.breakSubstring "<!DOCTYPE" bytes of
  (_, rest) | BS.null rest -> bytes
  (before, rest) -> before <> BS.replicate end 0x20 <> BS.drop end rest
    where
      end = close (0 :: Int) 0
      -- The subset in brackets may itself contain a '>'.
      close !depth !k
        | k >= BS.length rest = k
        | otherwise = case BS.index rest k of
            0x5B -> close (depth + 1) (k + 1) -- '['
            0x5D -> close (depth - 1) (k + 1) -- ']'
            0x3E | depth <= 0 -> k + 1 -- '>'
            _ -> close depth (k + 1)

-- | Decode predefined XML entities and numeric references to UTF-8.
-- Unknown or invalid references are left unchanged. Input without @&@
-- is returned without copying.
decodeEntities :: ByteString -> ByteString
decodeEntities v
  | BS.notElem 0x26 v = v
  | otherwise = BL.toStrict (B.toLazyByteString (go v))
  where
    go s =
      let (before, rest) = BS.break (== 0x26) s
       in if BS.null rest
            then B.byteString before
            else case entity rest of
              Just (b, after) -> B.byteString before <> b <> go after
               -- Preserve unknown or malformed references literally.
              Nothing -> B.byteString before <> B.word8 0x26 <> go (BS.drop 1 rest)

-- | One reference at the front of the input, and what is left after it.
entity :: ByteString -> Maybe (B.Builder, ByteString)
entity s
  | Just r <- BS.stripPrefix "&amp;" s = Just (B.word8 0x26, r)
  | Just r <- BS.stripPrefix "&lt;" s = Just (B.word8 0x3C, r)
  | Just r <- BS.stripPrefix "&gt;" s = Just (B.word8 0x3E, r)
  | Just r <- BS.stripPrefix "&quot;" s = Just (B.word8 0x22, r)
  | Just r <- BS.stripPrefix "&apos;" s = Just (B.word8 0x27, r)
  | Just r <- BS.stripPrefix "&#x" s = numeric 16 isHexDigit (fromIntegral . hexValue) r
  | Just r <- BS.stripPrefix "&#X" s = numeric 16 isHexDigit (fromIntegral . hexValue) r
  | Just r <- BS.stripPrefix "&#" s = numeric 10 isDigitByte (fromIntegral . hexValue) r
  | otherwise = Nothing
  where
    numeric base isPart value r =
      let ds = BS.takeWhile isPart r
          after = BS.drop (BS.length ds) r
          code = BS.foldl' (\acc w -> acc * base + value w) (0 :: Int) ds
       in if BS.null ds || not (BS.isPrefixOf ";" after) || not (isScalar code)
            then Nothing
            else Just (B.charUtf8 (chr code), BS.drop 1 after)
    -- Reject NUL, surrogates and out-of-range code points before calling chr.
    isScalar c = c > 0 && c <= 0x10FFFF && not (c >= 0xD800 && c <= 0xDFFF)
