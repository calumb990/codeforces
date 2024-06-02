{-# LANGUAGE Safe #-}

import Data.Array.MArray.Safe
import Data.Array.ST.Safe
import Control.Monad.ST

{-
  Creates 
-}
createIntMArray :: [Int] -> ST s (STArray s Int Int)
createIntMArray list = newListArray (0, length list) list :: ST s (STArray s Int Int)

readIntMArray :: STArray s Int Int -> Int -> ST s Int
readIntMArray = readArray

{-
  
-}
writeIntMArray :: STArray s Int Int -> Int -> Int -> ST s ()
writeIntMArray = writeArray

foo = do
    createIntMArray [1, 2, 3, 4] >>= (\r ->
        let monad = readIntMArray r 2
        in runST monad)