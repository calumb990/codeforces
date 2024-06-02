-- NUMBER THEORY --

{-
  Checks whether a double has no decimal and
  hence can be treated as an integer.
-}
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
  that are integers of the equation
  "ax + by = c" where "c" is a target constant
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

-------------------
