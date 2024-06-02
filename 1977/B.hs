import System.IO
import Data.Bits
import Data.Sequence

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

main :: IO ()
main = do
    list <- input
    let tests = read (head list) :: Int
    performTests tests

performTests :: Int -> IO ()
performTests 0 = return ()
performTests tests = do
    list <- input
    let n = read (head list) :: Int
    let result = binaryColour n False
    flushLine (show (Data.Sequence.length result))
    separate result
    performTests (tests-1)

separate :: Seq Int -> IO ()
separate (x:<|Empty) = flushLine (show x)
separate (x:<|xs) = do
    flush (show x ++ " ")
    separate xs

binaryColour :: Int -> Bool -> Seq Int
binaryColour 0 True = fromList [-1]
binaryColour 0 False = fromList [1]
binaryColour 1 True = fromList [-1]
binaryColour 1 False = fromList [1]
binaryColour n negate
    | n == n' = simplify normalResult
    | otherwise = simplify (colourResult >< outputResult)
  where
    oBit = 64 - countLeadingZeros n
    oValue = 2 ^ oBit
    n' = oValue - n
    pBit = 64 - countLeadingZeros n'
    pValue = 2 ^ pBit
    normalResult = outBits n 0 0 oBit negate
    colourResult = binaryColour n' (not negate)
    outputResult = outBits oValue 0 (Data.Sequence.length colourResult) (oBit+1) negate

outBits :: Int -> Int -> Int -> Int -> Bool -> Seq Int
outBits x y lowBit highBit negate
    | lowBit == highBit = fromList []
    | negate && xBit == 0 && yBit == 1 = 1 <| result
    | negate && xBit == 1 && yBit == 0 = -1 <| result
    | xBit == 0 && yBit == 1 = -1 <| result
    | xBit == 1 && yBit == 0 = 1 <| result
    | otherwise = 0 <| result
  where
    xBit = shiftR x lowBit .&. 1
    yBit = shiftR y lowBit .&. 1
    result = outBits x y (lowBit+1) highBit negate

simplify :: Seq Int -> Seq Int
simplify = simplify' 0

simplify' :: Int -> Seq Int -> Seq Int
simplify' x colour
    | x + 1 >= Data.Sequence.length colour = colour
    | index colour x == 1 && index colour (x+1) == -1 = trimZeroes negative
    | index colour x == -1 && index colour (x+1) == 1 = trimZeroes positive
    | otherwise = trimZeroes (simplify' (x+1) colour)
  where
    negative = simplify' (x+1) (deleteAt (x+1) (deleteAt (x+1) (insertAt x (-1) colour)))
    positive = simplify' (x+1) (deleteAt (x+1) (deleteAt (x+1) (insertAt x 1 colour)))

trimZeroes :: Seq Int -> Seq Int
trimZeroes = dropWhileR (== 0)
