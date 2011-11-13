module Text.Portable.ByteString.Char8 (lines) where

import Prelude as P hiding (lines)

import Data.ByteString.Char8 as C (ByteString)
import Data.ByteString.Unsafe (unsafeIndex, unsafeTake, unsafeDrop)

import qualified Data.ByteString as B

-- | Like the 'C.lines' function from Data.ByteString.Char8, but treat the
-- @\"\\r\\n\"@ and @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
lines :: ByteString -> [ByteString]
lines str | B.null str = []
          | otherwise  = let (line, rest) = breakNewline str
                          in line : lines rest

breakNewline :: ByteString -> (ByteString, ByteString)
breakNewline string = search 0
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
