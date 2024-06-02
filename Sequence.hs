import Data.Sequence as Seq
import Data.Maybe (fromJust)
import Data.Foldable (toList)

-- SEQUENCE --

{-
  Replaces an element at the nth index of a sequence
  by reconstructing the original sequence
-}
insert :: Int -> a -> Seq a -> Seq a
insert n item xs =
    front >< fromList [item] >< Seq.drop n xs
  where
    front = Seq.take (n-1) xs

{-
  Performs quicksort on a sequence of any value
  taking a function as its first argument that determines
  if a value becomes before the pivot and hence returns true
-}
quicksort :: (a -> a -> Bool) -> Seq a -> Seq a
quicksort f Seq.Empty = Seq.Empty
quicksort f (x:<|Seq.Empty) = fromList [x]
quicksort f (pivot:<|xs) = a >< fromList [pivot] >< b
  where
    a = quicksort f (fromList [x| x <- toList xs, f x pivot])
    b = quicksort f (fromList [x| x <- toList xs, not (f x pivot)])

--------------
