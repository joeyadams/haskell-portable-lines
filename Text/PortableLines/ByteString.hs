module Text.PortableLines.ByteString
    ( lines8
    ) where

import Prelude as P hiding (lines)

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Unsafe (unsafeIndex, unsafeTake, unsafeDrop)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Like the 'B8.lines' function from Data.ByteString.Char8, but treat the
-- @\"\\r\\n\"@ and @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
--
-- Input is assumed to be in ASCII or an ASCII-compatible encoding (at least
-- with respect to newline characters).  For example, UTF-8 is fine, but UTF-16
-- is not.
lines8 :: ByteString -> [ByteString]
lines8 str | B.null str = []
           | otherwise  = let (line, rest) = breakNewline8 str
                           in line : lines8 rest

breakNewline8 :: ByteString -> (ByteString, ByteString)
breakNewline8 string = search 0
    where
        len = B.length string

        byte_at i | i < len   = Just (string `unsafeIndex` i)
                  | otherwise = Nothing

        search i = case byte_at i of
            Nothing -> (string, B.empty)
            Just 10 -> (unsafeTake i string, unsafeDrop (i+1) string)
            Just 13 -> case byte_at (i+1) of
                Just 10 -> (unsafeTake i string, unsafeDrop (i+2) string)
                _       -> (unsafeTake i string, unsafeDrop (i+1) string)
            _       -> search (i+1)
