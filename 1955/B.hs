{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
import Data.Sequence as Seq
import Data.Maybe (fromJust)
import Data.Foldable (toList)

import System.IO

-- NUMBER THEORY --

{--
  Checks whether a double has no decimal and
  hence can be treated as an integer.
--}
isInt :: Double -> Bool
isInt x = floor x == ceiling x

{-
  Calculates the coefficients "x" and "y"
  of the equation "ax + by = g" where "g"
  is GCD(a, b), performing extended Eulid.
-}
extendedEuclid :: Int -> Int -> (Int, Int)
extendedEuclid 0 0 = (0, 0)
extendedEuclid 0 b = (0, 1)
extendedEuclid a 0 = (1, 0)
extendedEuclid a b = (y1, x1 - y1 * abs (div a b))
  where
    (x1, y1) = extendedEuclid b (mod a b)

{-
  Calculates the coefficients "x" and "y"
  of the equation "ax + by = c" where "c"
  is a target constant, solving the
  Diophantine equation for its first solution.
-}
singleDiophantine :: Int -> Int -> Int -> (Int, Int)
singleDiophantine 0 0 c = (0, 0)
singleDiophantine a b 0 = (0, 0)
singleDiophantine a b c = (xg * frac, yg * frac)
  where
    (xg, yg) = extendedEuclid a b
    frac = div c (gcd a b)

{-
  Calculates the coefficients "x" and "y"
  of the equation "ax + by = c" where "c"
  is a target constant, solving the 
  Diophantine equation for a solution in range.
-}
findDiophantine :: Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
findDiophantine a 0 0 xRange yRange = (0, 0)
findDiophantine 0 b 0 xRange yRange = (0, 0)
findDiophantine 0 0 c xRange yRange = (0, 0)
findDiophantine a b c (xStart, xEnd) (yStart, yEnd)
    | xStart > xEnd = (0, 0)
    | not (isInt k) = findDiophantine a b c (xStart + 1, xEnd) (yStart, yEnd)
    | yStart <= y && y <= yEnd = (xStart, y)
    | otherwise = findDiophantine a b c (xStart + 1, xEnd) (yStart, yEnd)

  where
    g = gcd a b
    (x0, y0) = singleDiophantine a b c
    k = fromIntegral (g * (xStart - x0)) / fromIntegral b :: Double
    y = y0 - (floor k * div a g)

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
performTests x = do
    list <- input
    let n = read (head list) :: Int
    let c = read (list !! 1) :: Int
    let d = read (list !! 2) :: Int
    let small = min c d
    let large = max c d

    list <- input
    let sortedList = Seq.sort (fromList (map read list :: [Int]))
    let a = Seq.index sortedList 0
    let combinations = fastConstruct a n small large

    if sortedList == combinations
        then flushLine "YES"
        else flushLine "NO"

    performTests (x-1)

fastConstruct :: Int -> Int -> Int -> Int -> Seq Int
fastConstruct = fastConstruct' (fromList []) (0, 0) (0, 0)

fastConstruct' :: Seq Int -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Int -> Seq Int
fastConstruct' xs (s, e) (i, j) a n small large =
    if i == (n-1) || j == 0
        then if e == (n-1)
            then fastConstruct' (xs|>value) (s+1, e) (s+1, e) a n small large
            else fastConstruct' (xs|>value) (s, e+1) (s, e+1) a n small large
        else if not (i >= n || j >= n)
            then fastConstruct' (xs|>value) (s, e) (i+1, j-1) a n small large
            else xs

  where
      value = large * i + small * j + a
