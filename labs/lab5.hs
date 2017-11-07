stringOfInteger :: Integer -> String
--precondition: Z 
--postcondition: En sträng av heltalet som input
--example:-42 -> "-42", 42 -> "42", 0 -> "0"
--variant: a minskar
stringOfInteger a | a < 0 = "-" ++ stringOfInteger (abs a)
stringOfInteger 0 = "0"
stringOfInteger 1 = "1"
stringOfInteger 2 = "2"
stringOfInteger 3 = "3"
stringOfInteger 4 = "4"
stringOfInteger 5 = "5"
stringOfInteger 6 = "6"
stringOfInteger 7 = "7"
stringOfInteger 8 = "8"
stringOfInteger 9 = "9"
stringOfInteger a =  (stringOfInteger (a `div` 10)) ++ (stringOfInteger (a `mod` 10))

searchString :: String -> String -> Integer
searchString x y = searchStringAux x y 0

searchStringAux :: String -> String -> Integer -> Integer 
--precondition:  Två strängar, a, b
--postcondition: index före b  är ekvivalent med a
--example: "jultomte" "tomte" == 3, "jultomte" "jul" == 0, "" "bil" == -1
--variant: (x:xs) minskar
searchStringAux [] [] _ = 0
searchStringAux [] _ acc = (-1)
searchStringAux (x:xs) y acc = if (take (length y) (x:xs)) == y then 0 + acc else searchStringAux xs y (acc+1)

--searchStringAux acc [] = acc
--searchStringAux acc (_:xs) =


--severian = binarytodecimal
--vodalus = binnum
 
