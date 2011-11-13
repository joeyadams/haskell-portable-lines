import Prelude hiding (catch, lines)

import Control.Exception

import qualified Data.ByteString.Char8 as B8
import qualified Text.Portable as P
import qualified Text.Portable.ByteString.Char8 as PB

test :: String -> IO ()
test str = print (P.lines str) `catch` \e -> print (e :: ErrorCall)

test_bytestring :: String -> IO ()
test_bytestring str = print (PB.lines $ B8.pack str) `catch` \e -> print (e :: ErrorCall)

tests :: [String]
tests =
    [ "Hello"
    , "Hello"
    , ""
    , "H"
    , "Hello\n"
    , "Hello\r"
    , "Hello\r\n"
    , "Hello\r\nbye"
    , "Hello\nbye"
    , "Hello\n\rbye"
    , "Hello\nworld\n"
    , "Hello\nworld\r\n"
    , "\r\n"
    , "\n"
    , "\r"
    , "\n\r"
    , "\r\n\r\n"
    ]

tests_string :: [String]
tests_string =
    [ "Hello" ++ undefined
    , "Hello\n" ++ undefined
    ]

indent :: IO a -> IO a
indent = (putStr "    " >>)

main :: IO ()
main = do
    putStrLn "Testing Text.PortableLines"
    mapM_ (indent . test) $ tests ++ tests_string

    putStrLn "Testing Text.PortableLines.ByteString.Char8"
    mapM_ (indent . test_bytestring) tests

    putStrLn "Done"
