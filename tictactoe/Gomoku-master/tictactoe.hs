type Move = (Int, Int)
type Board = [[Symbol]]
data Symbol = E | X | O deriving (Eq, Show)
 

validMove :: Board -> (Int, Int) -> Bool 
validMove b (x,y) 
        | validMoveAux b (x,y) = isEmpty b (x,y)
        | otherwise = False
            
            
validMoveAux :: Board -> (Int, Int) -> Bool
validMoveAux b (x, y)
            | x > 3 = False
            | x < 1 = False
            | y > 3 = False
            | y < 1 = False
            |otherwise = True
            

isEmpty :: Board -> (Int, Int) -> Bool
isEmpty b (x,y) = fos ((!!) b (x-1)) y
    where
        fos b y = if (!!) b (y-1) == E then True else False
        
-- [[E,E,E],[E,E,E],[E,E,E]]
