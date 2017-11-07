squareArea :: Double->Double
squareArea x = x ^ 2

circleArea :: Double->Double
circleArea x = pi * x ^ 2

squareCircleArea :: Double->Double
squareCircleArea x = (squareArea (2 * x) - circleArea (x))

cathetus :: Double->Double
cathetus x = x / (sqrt(2))

circleSquareArea :: Double->Double
circleSquareArea x = circleArea(x) - squareArea(cathetus (2 * x))


last3 :: String->String
last3 a = drop (length(a) - 3) a

rhymes :: String->String->Bool
rhymes a b = if length(a) < 3 || length(b) < 3 then a==b else last3 a == last3 b 

--rhymes :: String->String->Bool; rhymes a b = (last3 a == last3 b)

drJeep :: String -> String -> Bool
drJeep x y =
  not (length x < length y) && drop (length x - length y) x == y

--name for function should be endsWith. Name for x should be testee, name for y should be suffix.



