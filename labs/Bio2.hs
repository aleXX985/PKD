-- DO NOT MODIFY THE FOLLOWING LINE
module Bio(count, transcribe, reverseComplement, highestGC, translate, reversePalindromes) where
-- DO NOT MODIFY THE PREVIOUS LINE

import Test.HUnit  -- if this causes an error, type 'cabal install HUnit' at the command line

-- DO NOT MODIFY THE FOLLOWING DATA TYPES
type DNAString = String
type RNAString = String
type Protein = String
type Position = Int
type Length = Int
-- END OF DO NOT MODIFY ZONE

-- Question 1

count :: DNAString -> (Int, Int, Int, Int)
count [] = (0, 0, 0, 0)
count (x:xs)
  | x=='A' = (a+1, c, g, t)
  | x=='C' = (a, c+1, g, t)
  | x=='G' = (a, c, g+1, t)
  | x=='T' = (a, c, g, t+1)
    where (a, c, g, t) = count xs

-- Question 2

transcribe :: DNAString -> RNAString
transcribe [] = []
transcribe ('T':xs) = 'U':transcribe xs
transcribe (x:xs) = x:transcribe xs


-- Question 3

reverseComplement  :: DNAString -> DNAString
reverseComplement s = map complement (reverse s)
  where
    complement 'A' = 'T'
    complement 'T' = 'A'
    complement 'G' = 'C'
    complement 'C' = 'G'


-- Question 4

highestGC :: [DNAString] -> (Int, Double)
highestGC xs = 
  let
    gcs = map gc xs
    maxGc = maximum (gcs)
  in
    (elemIndex gcs maxGc, maxGc)

elemIndex :: (Eq a, Integral b) => [a] -> a -> b
elemIndex [] _ = 0
elemIndex (x:xs) y = if x==y then 0 else (elemIndex xs y) + 1

gc :: DNAString -> Double
gc s = (fromIntegral (length [ x | x <- s, x == 'C' || x == 'G' ]) / fromIntegral (length s)) * 100


-- Question 5

translate :: RNAString -> Maybe Protein
translate s = undefined


-- Question 6

reversePalindromes :: DNAString -> [(Position, Length)]
reversePalindromes s = undefined


-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.

-- Question 1 -- count
test1a = TestCase $ assertEqual "count CCCGCGTGACTCGGCCCTTTAAT" (3,9,5,6) (count "CCCGCGTGACTCGGCCCTTTAAT")

-- Question 2 -- transcribe
test2a = TestCase $ assertEqual "transcribe CCCGCGTGACTCGGCCCTTTAAT" "CCCGCGUGACUCGGCCCUUUAAU" (transcribe "CCCGCGTGACTCGGCCCTTTAAT")

-- Question 3 -- reverseComplement
test3a = TestCase $ assertEqual "reverseComplement CATGAG" "CTCATG" (reverseComplement "CATGAG")

-- Question 4 -- highestGC
specialEq (a,b) (c,d) = a == c && abs (b - d) < 0.000001

test4a = TestCase $ assertBool "highestGC [...]" (specialEq (0,60.869565217391305)
          (highestGC ["CCCGCGTGACTCGGCCCTTTAAT", "TCGACACTGACATAGGGACCATG", "TTTAAACTTGGAGGCCTAGTCAA"]))

-- Question 5 -- translate
test5a = TestCase $ assertEqual "translate UCGACACUGACAUAGGGACCAUG" (Just "STLT") (translate "UCGACACUGACAUAGGGACCAUG")
test5b = TestCase $ assertEqual "translate CAUGAG" Nothing (translate "CAUGAG")

-- Question 6 -- reversePalindromes
test6a = TestCase $ assertBool "reversePalindromes T... (0,4)" ((0,4) `elem` (reversePalindromes "TCGACACTGACATAGGGACCATG"))
test6b = TestCase $ assertBool "reversePalindromes T... (19,4)" ((19,4) `elem` (reversePalindromes "TCGACACTGACATAGGGACCATG"))


-- for running all the tests
runtests = runTestTT $ TestList [test1a, test2a, test3a, test4a, test5a, test5b, test6a, test6b]
