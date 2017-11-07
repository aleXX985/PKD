{- myInit list
   PRE: True
   POST: A list of everything except the last element of the original list.
   SIDE EFFECTS: None
   VARIANT: length of the list
   EXAMPLE: [1,2,3,4] = [1,2,3]
            ["hello","goodbye"] = ["hello"]
   
-}
myInit :: [a] -> [a]
myInit [] = []
myInit (x:[]) = []
myInit (x:xs) = (x:myInit xs)

{- fromDecimals ns
   PRE: True
   POST: The list converted into the corresponding integer value.
   SIDE EFFECTS: None
   VARIANT: Length of the list.
   EXAMPLE: [4,2] = 42
            [1,3,3,7] = 1337
            [] = 0
-}
fromDecimals :: [Integer] -> Integer
fromDecimals xs = fromDecimalsAux xs 1

{- fromDecimalsAux ns 1
   PRE: True
   POST: The list converted into the corresponding integer value.
   SIDE EFFECTS: None
   VARIANT: Length of the list.
   EXAMPLE: [4,2] 1 = 42
            [1,3,3,7] 1 = 1337
            [] 1 = 0
-}
fromDecimalsAux :: [Integer] -> Integer -> Integer
fromDecimalsAux [] _ = 0
fromDecimalsAux (n:[]) a = n * a
fromDecimalsAux ns a = fromDecimalsAux (init ns) (10*a) + last ns * a

{- squareOfEven1 ns
   PRE: True
   POST: A list of the squares of all even elements in ns
   SIDE EFFECTS: None
   VARIANT: Length of ns
   EXAMPLE: [0,1,2,3,4] = [0,4,16]
-}
squareOfEven1 :: [Integer] -> [Integer]
squareOfEven1 [] = []
squareOfEven1 (n:ns) 
                |odd n = squareOfEven1 ns
                |otherwise = (n*n:squareOfEven1 ns)

{- squareOfEven2 ns
   PRE: True
   POST: A list of the squares of all even elements in ns
   SIDE EFFECTS: None
   EXAMPLE: [0,1,2,3,4] = [0,4,16]
-}
squareOfEven2 :: [Integer] -> [Integer]
squareOfEven2 ns = [n*n | n <- ns, even n]







split :: [a] -> ([a],[a])
split xs =
  let
    l = length xs `div` 2
  in
   (take l xs, drop l xs)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | y < x = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let
    (xs1,xs2) = split xs
  in
   merge (mergeSort xs1) (mergeSort xs2)

insert k [] = [k]
insert k (x:xs) =
  if k < x then
    k : x : xs
  else
    x:(insert k xs)

insertionSortAux sorted [] = sorted
insertionSortAux sorted (x:xs) =
  insertionSortAux (insert x sorted) xs

insertionSort xs =
  insertionSortAux [] xs
