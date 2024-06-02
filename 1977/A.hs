import System.IO

-- SYSTEM --

{-
  Checks whether a double has no decimal and
  hence can be treated as an integer.
-}
isInt :: Double -> Bool
isInt x = floor x == ceiling x

{--
  Converts a line of standard input
  into an array of words as strings.
--}
input :: IO [String]
input = do
    let line = getLine
    line >>= \x -> return (words x)

{--
  Flushes a string to standard output
  forcing no newline buffering.
--}
flush :: String -> IO ()
flush text = do
    putStr text
    hFlush stdout

{--
  Flushes a string to standard output
  forcing no newline buffering.
--}
flushLine :: String -> IO ()
flushLine text = do
    putStr (text ++ ['\n'])
    hFlush stdout

------------

main :: IO ()
main = do
    list <- input
    let tests = read (head list) :: Int
    performTests tests

performTests :: Int -> IO ()
performTests 0 = return ()
performTests tests = do
    list <- input
    let n = read (list !! 0) :: Double
    let m = read (list !! 1) :: Double
    let x = (n + m) / 2
    let y = (n - m) / 2
    if (x >= 0 && y >= 0 && isInt x && isInt y)
        then flushLine "Yes"
        else flushLine "No"
    performTests (tests-1)
