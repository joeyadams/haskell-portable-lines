import Prelude hiding (catch, lines)

import Control.Exception
import Text.Portable

test :: String -> IO ()
test str = print (lines str) `catch` \e -> print (e :: ErrorCall)

main :: IO ()
main = mapM_ test
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
    , $ "Hello" ++ undefined
    , $ "Hello\n" ++ undefined
    ]
