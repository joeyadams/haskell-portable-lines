import Prelude hiding (catch, lines)

import Control.Exception

import qualified Data.ByteString.Char8 as B8
import qualified Text.Portable as P
import qualified Text.Portable.ByteString.Char8 as PB

printCatchException :: (Show a) => a -> IO ()
printCatchException = loop id . show
    where
        loop dl cons = do
            e <- try $ evaluate cons
            case e of
                Left ex -> putStrLn $
                    dl . ("*** Exception: " ++) . (show (ex :: ErrorCall) ++) $ []
                Right (x:xs)   -> loop (dl . (x:)) xs
                Right []       -> putStrLn $ dl []

test :: String -> IO ()
test = printCatchException . P.lines

test_bytestring :: String -> IO ()
test_bytestring = printCatchException . PB.lines . B8.pack

tests :: [String]
tests =
    [ "Hello"
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
