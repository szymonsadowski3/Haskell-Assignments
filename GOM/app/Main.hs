import Data.List
import Data.List.Split
import Data.Universe.Helpers
import Data.Ord
import Debug.Trace
import System.Process

clear = system "cls"
-- import MyTree

data Cell = 
        NotChecked
        |X
        |O deriving (Eq)  

toString NotChecked = "_"
toString X = "X"
toString O = "O"

oppositeCell X = O
oppositeCell O = X
oppositeCell _ = NotChecked


instance Show (Cell) where
    show = toString

readCell :: [Char] -> Cell
readCell "X" = X
readCell "x" = X
readCell "O" = O
readCell "o" = O
readCell "_" = NotChecked
readCell x = NotChecked

instance Show Board where
    show (Board rowsNum board) = intercalate "\n" [dispRow x | x <- board]

data Board = Board
    { rowsNumber    :: Integer
    , board2d       :: [[Cell]]
    }

generateRow n = [NotChecked | x <- [0..n]]
generateBoard n = Board n [generateRow n | x <- [0..n]]

-- toStrBoard (Board rowsNum board) = intercalate "\n" [dispRow x | x <- board]

elo = Board 2 [[X,X], [O,O]]
sofull = Board 19 [[X,X,O,X,X,X,O,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,O,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X]]
somix = Board 19 [[NotChecked,NotChecked,O,X,X,X,O,NotChecked,X,X,X,X,X,X,X,X,X,X,X], [NotChecked,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,O,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X]]
soblank = Board 19  [[NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked], [NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked]]
tiny = Board 4 [[NotChecked, NotChecked, NotChecked, NotChecked],[NotChecked, NotChecked, NotChecked, NotChecked],[NotChecked, NotChecked, NotChecked, NotChecked],[NotChecked, NotChecked, NotChecked, NotChecked]]
mini = Board 2 [[NotChecked, NotChecked], [NotChecked, NotChecked]]

eloo = generateBoard 13


tenxten = Board 10 [[NotChecked, NotChecked], [NotChecked, NotChecked]]

dispRow row = intercalate " " [show x | x <- row]



--UTILS
cwRotate = map reverse . transpose

diffs [] = []
diffs ls = zipWith (-) (tail ls) ls

replace :: Int -> a -> [a] -> [a]
replace n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

changeElem row col x xs =
    let row_to_replace_in = xs !! row
        modified_row = replace col x row_to_replace_in
    in replace row modified_row xs
--

--EVALUATION FUNCTION
evalChain 2 = 1.0
evalChain 3 = 5.0
evalChain 4 = 100.0
evalChain 5 = 5000.0
evalChain x = 0

indicesInRows board symb = [elemIndices (symb) x | x <- board]
indicesDiffsInRows board symb = [diffs x | x <- indicesInRows board symb]

getSymbGroups board symb = concat [splitOneOf [2..19] x | x <- (indicesDiffsInRows board symb)]
getChainsRowWise board symb = [(length x) + 1 | x <- (getSymbGroups board symb)]


diagonalsCw board = diagonals (cwRotate board)

evalBoardRowWise board symb = sum $ fmap evalChain $ getChainsRowWise board symb

--CUT
takeRange cutRatio array = (drop cutRatio . take ((length array) - cutRatio) $ array)
getCutBoard cutRatio board = concat [takeRange cutRatio x | x <- takeRange cutRatio (board)]
--
middle (Board rowsNum board) symb = (getCutBoard  ((fromIntegral $ quot (rowsNum) 2)-2) board)

evalMid (Board rowsNum board) symb = fromIntegral $ length $ filter (==symb) $ middle (Board rowsNum board) symb

evalBoard :: Fractional a => Board -> Cell -> a
evalBoard (Board rowsNum board) symb =  (evalMid (Board rowsNum board) symb) + (evalBoardRowWise board symb) + (evalBoardRowWise (cwRotate board) symb) + (evalBoardRowWise (diagonals board) symb) + (evalBoardRowWise (diagonalsCw board) symb)


availMovesBasis (Board rowsNum board) = [(elemIndices NotChecked x) | x <- board]
availMoves (Board rowsNum board) = concat [zip (replicate (length $ snd x) (fst x)) (snd x) | x <- zip [0..] (availMovesBasis (Board rowsNum board))]

availBoardsRapid (Board rowsNum board) whoseMove state = concat [zip (replicate (length $ snd x) (fst x)) (snd x) | x <- zip [0..] (availMovesBasis (Board rowsNum board))]

availNextBoards (Board rowsNum board) whoseMove = [Board rowsNum (changeElem (fst x) (snd x) whoseMove board) | x <- availMoves (Board rowsNum board)]

--GAME TREE
generateGameTreeDepths whoseMove boardCls 1 = Node (evalBoard boardCls X, boardCls) [Node (evalBoard x X, x) [] | x <- (availNextBoards boardCls whoseMove)]
generateGameTreeDepths whoseMove boardCls depth = Node (evalBoard boardCls X, boardCls) [generateGameTreeDepths (oppositeCell whoseMove) x (depth - 1)| x <- (availNextBoards boardCls whoseMove)]
-- evalRowWise boardCls symb

minimalizationLayer whoseMove boardCls --trtail
        | whoseMove == X = [(minimumBy (comparing (byF)) (getTail (fst x)), snd x) | x <- zip (trtail) [0..(length trtail)]]
        | otherwise =  [(maximumBy (comparing (byF)) (getTail (fst x)), snd x) | x <- zip (trtail) [0..(length trtail)]]          
        where trtail = getTail (generateGameTreeDepths whoseMove boardCls 3)

bestMove whoseMove boardCls --trtail
        | whoseMove == X = (maximumBy (comparing (byComplex))) (minimalizationLayer whoseMove boardCls)
        | otherwise =  (minimumBy (comparing (byComplex))) (minimalizationLayer whoseMove boardCls)


minimax (Node v [])    = v
minimax (Node v tail) = myNegate (minimumBy (comparing byComplex2) (map minimax tail))

-- minimax (Node v [])    = (Node v [])
-- minimax (Node v tail) = myNegate (minimumBy (comparing (byComplex)) (map (minimax) tail))

-- myNegate (Node (x,board) tail) = (Node (-x,board) tail)
myNegate (x,board) = (-x,board)
byF (Node (x,board) tail) = x

byComplex ((Node (x,board) tail), index) = x
byComplex2 (x,board) = x


mainLoop whoseMove boardState = do
    -- let tr = 
    let trtail = getTail (generateGameTreeDepths whoseMove boardState 1)
    putStrLn ("")
    putStrLn ("")
    putStrLn (show boardState)
    let choice = snd (bestMove whoseMove boardState)
    let nextBoard = snd (getNode (trtail!!choice))
    mainLoop (oppositeCell whoseMove) nextBoard


main :: IO ()
main = do  
    mainLoop X eloo
    putStrLn ("ELO")

data MTree a = Node a ([MTree a]) | Empty deriving (Show, Eq)

getNodes (Empty) = []
getNodes (Node v tail) = [v]++(concat [getNodes x | x <- tail])

isLeaf (Node v []) = True
isLeaf _ = False

getLeaves (Node v []) = [v]
getLeaves (Node v tail) = concat [getLeaves x | x <- tail]

getNode (Node v _) = v

getTail (Node v tail) = tail


instance Functor (MTree) where
    fmap _ Empty = Empty
    fmap f (Node v branches) = Node (f v) ([fmap f x | x <- branches])