import Prelude hiding (catch, lines)

import Control.Exception

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Text.PortableLines as P
import qualified Text.PortableLines.ByteString as PB
import qualified Text.PortableLines.ByteString.Lazy as PL

printCatch :: (Show a) => a -> IO ()
printCatch = loop id . show
    where
        loop dl cons = do
            e <- try $ evaluate cons
            case e of
                Left ex -> putStrLn $
                    dl . ("*** Exception: " ++) . (show (ex :: ErrorCall) ++) $ []
                Right (x:xs)   -> loop (dl . (x:)) xs
                Right []       -> putStrLn $ dl []

-- Tests for all string types
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

-- Tests for 'String' only
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
    mapM_ (indent . printCatch . P.lines) $ tests ++ tests_string

    putStrLn "Testing Text.PortableLines.ByteString"
    mapM_ (indent . printCatch . PB.lines8) $ map B8.pack tests

    putStrLn "Testing Text.PortableLines.ByteString.Lazy"
    mapM_ (indent . printCatch . PL.lines8) $ map L8.pack tests

    putStrLn "Done"
