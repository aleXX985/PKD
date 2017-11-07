
class Size s where 
    size :: s -> Integer
	
newtype F a = F (Int, a)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Size Int where
    size _ = 4

instance Size Integer where
    size n = ceiling (logBase 2 (fromInteger n) / 8.0) + 1

instance Size Bool where
    size _ = 1

instance Size Char where
    size _ = 2

instance Size () where
    size _ = 1

instance Size a => Size [a] where
    size l = foldl (\b h -> 1+b+size h) 1 l

instance Size a => Size (F a) where
    size (F a) = size a

instance (Size a, Size b) => Size (a, b) where
    size (a, b) = size a + size b

instance (Size a, Size b, Size c) => Size (a, b, c) where
    size (a, b, c) = size a + size b + size c

instance (Size a) => Size (Tree a) where
    size (Leaf a) = 1 + size a
    size (Branch a b) = size a + size b + 1



-- 10 :: Int = 4

-- 4 ^ 200 :: Integer = 51

-- ((), ()) = 2

-- "World" = 16

-- F (10, 4 ^ 200) :: F Integer = 55

-- ([1,2,3], "Hello World", Branch (Leaf (1, "Hello")) (Leaf (10000, "World"))) :: ([Int], String, Tree (Int, String)) = 92



