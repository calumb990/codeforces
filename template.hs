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