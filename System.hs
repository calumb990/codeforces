import System.IO

-- SYSTEM --

{--
  Converts a line of standard input
  into an array of words as strings
--}
input :: IO [String]
input = do
    let line = getLine
    line >>= \x -> return (words x)

{--
  Flushes a string to standard output
  forcing no newline buffering
--}
flush :: String -> IO ()
flush text = do
    putStr text
    hFlush stdout

{--
  Flushes a string to standard output
  forcing no newline buffering
--}
flushLine :: String -> IO ()
flushLine text = do
    putStr (text ++ ['\n'])
    hFlush stdout

------------
