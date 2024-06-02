import Data.Char

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