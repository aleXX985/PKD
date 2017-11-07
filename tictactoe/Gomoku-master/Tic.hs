import Control.Exception
import Control.Monad
import Prelude hiding(catch)
import System.Exit (exitSuccess)



data Symbol = E | X | O deriving (Eq, Show)

{- Move
  Representation Convention: represents a move. The first element of the tuple
    is the pile to move from, the second element is the amount to move.
  Representation Invariant: The first element refers to a valid pile. The second >= 0
-}
type Move = (Int, Int) 

type Player = (Symbol, Move)

{- Game State
  Representation Convention: represents the number of stones in the three piles.
  Representation Convention: each element of the tuple >= 0.
-}
type Board = [[Symbol]]


empty     = [[E,E,E],
			 [E,E,E],
			 [E,E,E]]

testBoard = [[X,E,O],
			 [E,X,O],
			 [O,E,E]]

switch :: Symbol -> Symbol
switch x 
    | x == X = O
    | x == O = X

showplayer :: Symbol -> String
showplayer x
    | x == X = "X"
    | x == O = "O"


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
validMove :: Board -> Player -> Bool 
validMove b (_,(x,y)) 
        | validMoveAux b (x,y) = isEmpty b (x,y)
        | otherwise = False
            
          
validMoveAux :: Board -> (Int, Int) -> Bool
validMoveAux b (x, y)
            | x > 3 = False
            | x < 1 = False
            | y > 3 = False
            | y < 1 = False
			| otherwise = True

isEmpty :: Board -> (Int, Int) -> Bool
isEmpty b (x,y) = fos ((!!) b (x-1)) y
	where
		fos b y = if (!!) b (y-1) == E then True else False



--[[E,E,E],[E,X,E],[E,E,E]]
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





{- printGameState gs
   Purpose: To print a game state
   Pre: None
   Post: True
   Side-effect: Displays game state to standard output
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

{-  victory board
    PRE:            True
    POST:           True if victory is achieved, otherwise False
-}
victory :: Board -> Bool
victory ([x1,x2,x3]:[y1,y2,y3]:[z1,z2,z3]:[])  
                    | [x1,x2,x3] == [X,X,X] || [x1,x2,x3] == [O,O,O] || 
					  [y1,y2,y3] == [X,X,X] || [y1,y2,y3] == [O,O,O] || 
                      [z1,z2,z3] == [X,X,X] || [z1,z2,z3] == [O,O,O] = True
                    | [x1,y1,z1] == [X,X,X] || [x1,y1,z1] == [O,O,O] || 
					  [x2,y2,z2] == [X,X,X] || [x2,y2,z2] == [O,O,O] ||
                      [x3,y3,z3] == [X,X,X] || [x3,y3,z3] == [O,O,O] = True
                    | [x1,y2,z3] == [X,X,X] || [x1,y2,z3] == [O,O,O] || 
					  [x3,y2,z1] == [X,X,X] || [x3,y2,z1] == [O,O,O] = True
					| otherwise = False

ai :: Board -> Move
ai ([x1,x2,x3]:[y1,y2,y3]:[z1,z2,z3]:[]) 
        | ([x2,x3] == [O,O] || [y1,z1] == [O,O] || [y2,z3] == [O,O]) && x1 == E = (1,1)
        | ([x1,x3] == [O,O] || [y2,z2] == [O,O])                     && x2 == E = (1,2)
        | ([x1,x2] == [O,O] || [y3,z3] == [O,O] || [y2,z1] == [O,O]) && x3 == E = (1,3)
        | ([x1,z1] == [O,O] || [y2,y3] == [O,O])                     && y1 == E = (2,1)
        | ([x2,z2] == [O,O] || [y1,y3] == [O,O] || 
           [x1,z3] == [O,O] || [x3,z1] == [O,O])                     && y2 == E = (2,2)
        | ([y1,y2] == [O,O] || [x3,z3] == [O,O])                     && y3 == E = (2,3)
        | ([z2,z3] == [O,O] || [x1,y1] == [O,O] || [x3,y2] == [O,O]) && z1 == E = (3,1)
        | ([z1,z3] == [O,O] || [x2,y2] == [O,O])                     && z2 == E = (3,2)
        | ([z1,z2] == [O,O] || [x3,y3] == [O,O] || [x1,y2] == [O,O]) && z3 == E = (3,3)

        | ([x2,x3] == [X,X] || [y1,z1] == [X,X] || [y2,z3] == [X,X]) && x1 == E = (1,1)
        | ([x1,x3] == [X,X] || [y2,z2] == [X,X])                     && x2 == E = (1,2)
        | ([x1,x2] == [X,X] || [y3,z3] == [X,X] || [y2,z1] == [X,X]) && x3 == E = (1,3)
        | ([x1,z1] == [X,X] || [y2,y3] == [X,X])                     && y1 == E = (2,1)
        | ([x2,z2] == [X,X] || [y1,y3] == [X,X] || 
           [x1,z3] == [X,X] || [x3,z1] == [X,X])                     && y2 == E = (2,2)
        | ([y1,y2] == [X,X] || [x3,z3] == [X,X])                     && y3 == E = (2,3)
        | ([z2,z3] == [X,X] || [x1,y1] == [X,X] || [x3,y2] == [X,X]) && z1 == E = (3,1)
        | ([z1,z3] == [X,X] || [x2,y2] == [X,X])                     && z2 == E = (3,2)
        | ([z1,z2] == [X,X] || [x3,y3] == [X,X] || [x1,y2] == [X,X]) && z3 == E = (3,3)

        | y2 == E = (2,2)

        | x3 == E = (1,3)
        | z1 == E = (3,1)
        | z3 == E = (3,3)
        | x1 == E = (1,1)

        | z2 == E = (3,2)
        | x2 == E = (1,2)
        | y3 == E = (2,3)
        | y1 == E = (2,1)


{-  boardFull board
    Pre:            True
    Post:           Whether the board is full or not
-}
boardFull :: Board -> Bool
boardFull b
			| (elem True . map (elem E)) b = False
			| otherwise = True

---------------MAIN---------------------------------------------------------------------------------------------------------------------



{- gameState
   Purpose: Generate a fresh game state
   Pre: None
   Post: A valid game state
   Side effect: None (at present)
-}
newBoard :: IO Board
newBoard = return empty




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
  play gameBoard X

{- play gs
   Purpose: Play the g
   Pre: gs is valid and not the victory state
   Post: True -- it never returns
   Side-effect: The game interaction 
-}
play gameBoard player = do
  printBoard gameBoard
  newGameBoard <- (if player == X then playerMove else aiMove) gameBoard player
  if victory newGameBoard 
	then do
		printBoard newGameBoard
		putStrLn $ (showplayer player) ++ " WINS"
  		return ()
	else if boardFull newGameBoard
		then do
			printBoard newGameBoard
			putStrLn "DRAW"
  			return ()		
		else do
			play newGameBoard (switch player)


{- playerMove gs
   Purpose: Perform the player's move
   Pre: gs is valid and not the victory state
   Post: a new game state
   Side-effect: Displays a description of the players's move
-}
playerMove :: Board -> Symbol -> IO Board
playerMove gameBoard currentPlayer = do
  putStrLn $ (showplayer currentPlayer) ++ ", your move."
  move <- readMove
  if validMove gameBoard (currentPlayer, move) then 
    return $ playMove gameBoard (currentPlayer, move)
   else do
    putStrLn "Invalid Move."
    playerMove gameBoard currentPlayer


{- playerMove gs
   Purpose: Perform the player's move
   Pre: gs is valid and not the victory state
   Post: a new game state
   Side-effect: Displays a description of the players's move
-}
aiMove :: Board -> Symbol -> IO Board
aiMove gameBoard currentPlayer = do
  putStrLn $ (showplayer currentPlayer) ++ ", your move."
  if validMove gameBoard (currentPlayer, (ai gameBoard)) then 
    return $ playMove gameBoard (currentPlayer, (ai gameBoard))
   else do
    putStrLn "Invalid Move."
    aiMove gameBoard currentPlayer

{- readMove
   Purpose: Reads a move from standard input
   Post: A move object 
   Side-effects: Reads one or more lines from standard input 
-}
readMove :: IO Move -- reads input from
readMove = do
  catch (do
    line <- getLine 
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
       putStrLn "Invalid input. Correct format: (pileNumber,amount)"
       readMove) :: SomeException -> IO Move)

