import System.IO
import Data.Bits
import Data.Char

-- SYSTEM --

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

-- TEXT --

{-
  Removes whitespace from the
  front and back of a string
-}
trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

{-
  Replaces an item in a string
  at a certain index provided
-}
replace :: String -> Int -> Int -> String -> String
replace is start size xs = take start xs ++ is ++ drop (start+size) xs

{-
  Takes a substring from a string
  using two indexes as the range
-}
substring :: Int -> Int -> String -> String
substring start size = take size . drop start

----------

main :: IO ()
main = do
    list <- input
    let tests = read (head list) :: Int
    performTests tests

performTests :: Int -> IO ()
performTests 0 = return ()
performTests tests = do
    list <- input
    let n = read (list !! 0) :: Int
    let result = binaryColour n False
    flushLine (show (numbers result))
    separate result
    performTests (tests-1)

separate :: String -> IO ()
separate (x:[]) = flushLine [x]
separate (x:xs) = do
    if (x == '-')
        then flush [x]
        else flush (x : " ")
    separate xs

binaryColour :: Int -> Bool -> String
binaryColour 0 True = "-1"
binaryColour 0 False = "1"
binaryColour 1 True = "-1"
binaryColour 1 False = "1"
binaryColour n negate
    | n == n' = simplify normalResult
    | otherwise = simplify (colourResult ++ outputResult)
  where
    oBit = 64 - countLeadingZeros n
    oValue = 2 ^ oBit
    n' = oValue - n
    pBit = 64 - countLeadingZeros n'
    pValue = 2 ^ pBit
    normalResult = outBits n 0 0 oBit negate
    colourResult = binaryColour n' (not negate)
    outputResult = outBits oValue 0 (numbers colourResult) (oBit+1) negate

outBits :: Int -> Int -> Int -> Int ->  Bool -> String
outBits x y lowBit highBit negate
    | lowBit == highBit = ""
    | negate && xBit == 0 && yBit == 1 = "1" ++ result
    | negate && xBit == 1 && yBit == 0 = "-1" ++ result
    | xBit == 0 && yBit == 1 = "-1" ++ result
    | xBit == 1 && yBit == 0 = "1" ++ result
    | otherwise = "0" ++ result
  where
    xBit = (shiftR x lowBit) .&. 1
    yBit = (shiftR y lowBit) .&. 1
    result = outBits x y (lowBit+1) highBit negate

simplify :: String -> String
simplify = simplify' 0

simplify' :: Int -> String -> String
simplify' x colour
    | x + 2 >= length colour = colour
    | substring x 3 colour == "1-1" = trimZeroes (simplify' (x+1) negative)
    | substring x 3 colour == "-11" = trimZeroes (simplify' (x+1) positive)
    | otherwise = trimZeroes (simplify' (x+1) colour)
  where
    negative = replace "-10" x 3 colour
    positive = replace "10" x 3 colour

trimZeroes :: String -> String
trimZeroes xs = (reverse (dropWhile (\c -> c == ' ' || c == '0') (reverse xs)))

numbers :: String -> Int
numbers xs = numbers' xs 0

numbers' :: String -> Int -> Int
numbers' [] c = c
numbers' (x:xs) c
    | x == '1' || x == '0' = numbers' xs (c+1)
    | otherwise = numbers' xs c
