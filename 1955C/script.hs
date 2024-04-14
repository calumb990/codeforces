{-# LANGUAGE ViewPatterns #-}

----------- TEMPLATE -------------

import System.IO
import Data.Sequence as Seq

{--
Drops n elements from the back
of a list rather than the front
--}
dropReverse :: Int -> [a] -> [a]
dropReverse n = Prelude.reverse . Prelude.drop n . Prelude.reverse

{--
Slices a list in order to retrieve
a sublist of a certain start index and length.
--}
slice :: Int -> Int -> [a] -> [a]
slice start count xs = Prelude.take count (Prelude.drop start xs)

{--
Replaces an element at the nth index of a list
by reconstructing the original list
--}
insert :: Int -> a -> Seq a -> Seq a
insert n item xs =
    front >< fromList [item] >< Seq.drop n xs
  where
    front = Seq.take (n-1) xs

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

----------------------------------

main :: IO ()
main = do
    list <- input
    let tests = read (head list) :: Int
    performTests tests

performTests :: Int -> IO ()
performTests 0 = return ()
performTests x = do
    list <- input
    let ships = read (head list) :: Int
    let attacks = read (list !! 1) :: Int
    list <- input
    let durability = map read list :: [Int]
    let leftDamage = (attacks + 1) `div` 2
    let rightDamage = attacks `div` 2
    let (leftShips, ys) = sinkShips 0 leftDamage (fromList durability)
    let (rightShips, zs) = sinkShips 0 rightDamage (Seq.reverse ys)
    flushLine (show (leftShips + rightShips))
    performTests (x-1)

sinkShips :: Int -> Int -> Seq Int -> (Int, Seq Int)
sinkShips sunk 0 ships = (sunk, ships)
sinkShips sunk damage (Seq.viewl -> Seq.EmptyL) = (sunk, empty)
sinkShips sunk damage (ship :<| ships)
    | damage >= ship = sinkShips (sunk+1) (damage-ship) (Seq.drop 0 ships)
    | damage < ship = (sunk, insert 0 (ship-damage) ships)
