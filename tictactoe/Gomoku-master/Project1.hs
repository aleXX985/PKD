import Control.Exception

type Player = (Symbol, Move)
type Move = (Int, Int)
data Symbol = E | X | O deriving (Show, Eq)
type Board = [[Symbol]]
--data Board = Leaf Symbol | Node [Board] deriving (Show, Eq)

{- Game State
  Representation Convention: represents the number of stones in the three piles.
  Representation Convention: each element of the tuple >= 0.
-}

empty     = [[E,E,E],
             [E,E,E],
             [E,E,E]]

test1 = [[X,E,O],
         [E,X,O],
         [O,E,X]]
              
{-
    +---+---+---+
    | X |   | O |  
    +---+---+---+
    |   | X | O |
    +---+---+---+
    | O |   | X |
    +---+---+---+
-}

test2 = [[X,X,E],
         [O,X,E],
         [O,O,E]]

{-
    +---+---+---+
    | X | X |   |  
    +---+---+---+
    | O | X |   |
    +---+---+---+
    | O | O |   |
    +---+---+---+
-}


{- printGameState gs
   Purpose: To print a game state
   Pre: None
   Post: True
   Side-effect: Displays game state to standard output
-}



{-  readMove
    PRE:            True
    POST:           A pair representing the coordinates of the selected position
    SIDE EFFECTS:   Requests a move from the user, also may result in an exception
-}
readMove :: IO Move
readMove = do
    catch (do
        coordinates <- getLine
        evaluate (read coordinates))
        ((\_ -> do 
            putStrLn "Invalid input. Please format like this: (row, column)"
            readMove) :: SomeException -> IO Move)

{-  printBoard board
    PRE:            True
    POST:           None
    SIDE EFFECTS:   Prints the current board state
-}
printBoard :: Board -> IO ()
printBoard b = do
    putStrLn $                "+---+---+---+"
    putStrLn $ "| " ++ (showpos b (1,1)) ++ " | " ++ (showpos b (1,2)) ++ " | " ++ (showpos b (1,3)) ++ " |"
    putStrLn $                "+---+---+---+"
    putStrLn $ "| " ++ (showpos b (2,1)) ++ " | " ++ (showpos b (2,2)) ++ " | " ++ (showpos b (2,3)) ++ " |"
    putStrLn $                "+---+---+---+"
    putStrLn $ "| " ++ (showpos b (3,1)) ++ " | " ++ (showpos b (3,2)) ++ " | " ++ (showpos b (3,3)) ++ " |"
    putStrLn $                "+---+---+---+"



{-  isEmpty board move
    PRE:            True
    POST:           Whether symbol is E or X/O
-}
isEmpty :: Board -> (Int, Int) -> Bool
isEmpty b (x,y) = fos ((!!) b (x-1)) y
    where
        fos b y = if (!!) b (y-1) == E then True else False


{-  showPos board move
    PRE:            True
    POST:           The symbol of the position at move
-}
showpos :: Board -> (Int, Int) -> String
showpos b (x,y) = fos ((!!) b (x-1)) y
    where
        fos b y = case (!!) b (y-1) of 
                                     X -> "X"
                                     O -> "O" 
                                     E -> " "



{-  validMove board move
    PRE:            True
    POST:           True if move is within bounds and 
-}
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
            | otherwise = True


{-  victory board
    PRE:            True
    POST:           True if victory is achieved, otherwise False
-}
victory :: Board -> Bool
victory ([x1,x2,x3]:[y1,y2,y3]:[z1,z2,z3]:[])  
                    | [x1,x2,x3] == [X,X,X] || [x1,x2,x3] == [O,O,O] || [y1,y2,y3] == [X,X,X] || [y1,y2,y3] == [O,O,O] || 
                        [z1,z2,z3] == [X,X,X] || [z1,z2,z3] == [O,O,O] = True
                    | [x1,y1,z1] == [X,X,X] || [x1,y1,z1] == [O,O,O] || [x2,y2,z2] == [X,X,X] || [x2,y2,z2] == [O,O,O] ||
                        [x3,y3,z3] == [X,X,X] || [x3,y3,z3] == [O,O,O] = True
                    | [x1,y2,z3] == [X,X,X] || [x1,y2,z3] == [O,O,O] || [x3,y2,z1] == [X,X,X] || [x3,y2,z1] == [O,O,O] = True
                    | otherwise = False

{- playmove state move 
   Purpose: To update the board state after making a move
   Pre: validMove state move
   Post: A valid game state
-}
playMove :: Board -> Player -> Board
playMove  b (x, m) = insert b x m 

insert :: Board -> Symbol -> Move -> Board
insert []     z (n,n') = [] 
insert (x:xs) z (1,n') = (insert' x z n'):(insert xs z (0,n'))
insert (x:xs) z (n,n') = x:(insert xs z (n-1,n'))



insert' :: [Symbol] -> Symbol -> Int -> [Symbol]
insert' []     z n = [] 
insert' (x:xs) z 1 = z:(insert' xs z (0))
insert' (x:xs) z n = x:(insert' xs z (n-1))


{-  genGameState a
    PRE:            True
    POST:           A board of size a*a
    SIDE EFFECTS:   Takes an integer and then prints the board
-}
genGameState :: IO Int -> IO Board
genGameState n = undefined

{- main
    Purpose: run the game
    Pre: None
    Post: True
    Side-effects: Quite a lot, actually
 -}
 main :: IO ()
 main = do 
   putStrLn "Welcome to TicTacToe."
   gameBoard <- newBoard
   play gameBoard
 
 {- play gs
    Purpose: Play the g
    Pre: gs is valid and not the victory state
    Post: True -- it never returns
    Side-effect: The game interaction 
 -}
 play gameBoard = do
   printBoard gameBoard
   newGameBoard <- playerMove gameBoard
   do
       play newGameBoard      
 
 {- playerMove gs
    Purpose: Perform the player's move
    Pre: gs is valid and not the victory state
    Post: a new game state
    Side-effect: Displays a description of the players's move
 -}
 playerMove :: Board -> IO Board
 playerMove gameBoard = do
   putStrLn "Your move."
   move <- readMove
   if isEmpty gameBoard move then 
     return $ insert gameBoard X move
    else do
     putStrLn "Invalid Move."
     playerMove gameBoard
 

{-  boardFull board
    Pre:            True
    Post:           Whether the board is full or not
-}
boardFull :: Board -> Bool
boardFull b | victory b = True
            | (elem True . map (elem E)) b = False
