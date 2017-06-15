
--{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.List.Split
import Data.Universe.Helpers
import MyTree

--showing board without escaping: putStrLn (show filledB)

--UTIL
replaceElem n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs
---

--data
testB = generateBoard 361
filledB = [X , X , X , O , X , X , NotChecked , X , NotChecked , X , X , X , O , X , X , NotChecked , O , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , O , O , X , NotChecked , O , X , NotChecked , O , O , X , O , X , X , NotChecked , X , X , NotChecked , O , X , X , NotChecked , NotChecked , O , NotChecked , NotChecked , X , NotChecked , X , NotChecked , O , O , NotChecked , NotChecked , O , O , O , O , X , NotChecked , O , NotChecked , NotChecked , NotChecked , X , NotChecked , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , O , O , NotChecked , X , X , O , X , X , X , O , NotChecked , NotChecked , NotChecked , NotChecked , X , NotChecked , X , O , NotChecked , NotChecked , NotChecked , X , X , X , NotChecked , O , X , NotChecked , O , O , X , O , O , O , NotChecked , O , O , X , NotChecked , X , O , X , X , O , O , NotChecked , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , NotChecked , X , NotChecked , X , X , O , X , X , O , O , NotChecked , NotChecked , NotChecked , NotChecked , O , NotChecked , NotChecked , O , NotChecked , O , X , X , X , NotChecked , X , O , O , X , NotChecked , NotChecked , NotChecked , O , O , X , O , O , X , NotChecked , NotChecked , NotChecked , NotChecked , X , X , NotChecked , NotChecked , O , X , O , X , X , O , O , NotChecked , NotChecked , X , O , X , NotChecked , X , X , NotChecked , X , O , NotChecked , NotChecked , X , X , O , NotChecked , O , NotChecked , NotChecked , X , X , O , NotChecked , O , X , O , X , O , O , NotChecked , X , X , O , X , NotChecked , O , O , O , X , NotChecked , X , O , NotChecked , X , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , X , NotChecked , X , O , X , O , O , O , NotChecked , NotChecked , O , O , NotChecked , NotChecked , X , NotChecked , O , NotChecked , NotChecked , O , NotChecked , O , X , O , NotChecked , NotChecked , NotChecked , NotChecked , O , O , X , O , NotChecked , X , NotChecked , X , O , X , X , NotChecked , O , NotChecked , X , NotChecked , NotChecked , NotChecked , X , O , O , X , NotChecked , X , NotChecked , NotChecked , NotChecked , O , NotChecked , O , NotChecked , X , O , NotChecked , NotChecked , O , NotChecked , X , NotChecked , O , X , NotChecked , O , NotChecked , O , O , X , O , O , NotChecked , NotChecked , O , O , X , O , X , X , X , O , O , O , O , NotChecked , X , X , O , NotChecked , O , X , O , X , X , X , NotChecked , O , NotChecked , X , X , X , X , O , O , O , X , X , NotChecked , NotChecked , X , X , O , O , NotChecked , NotChecked , O , NotChecked , NotChecked , NotChecked , NotChecked , X , O , X , X]
list = [1..9]
tiny = [X,NotChecked,NotChecked,NotChecked]
tiny2 = [X,X,X,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked,NotChecked]
list3 = [O,X,NotChecked,X,O,NotChecked,NotChecked,X,O]
soblank = [NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked ]
sofull = [X , O , O , X , O , O , O , X , X , O , O , X , O , O , X , X , X , X , X , X , X , O , O , X , O , X , X , O , X , O , O , O , X , X , O , O , O , X , X , X , X , X , X , O , O , X , X , X , O , X , O , O , O , X , O , X , X , O , O , X , O , X , X , X , O , O , O , O , O , O , X , O , O , X , O , O , X , X , O , X , O , X , X , X , X , X , X , O , O , O , X , X , X , O , X , O , O , X , X , X , X , O , X , O , O , O , X , X , O , O , X , O , X , X , X , X , X , X , O , O , O , X , O , X , X , X , O , X , O , X , O , O , O , O , X , X , X , X , O , X , O , O , O , X , O , X , X , O , O , X , X , O , O , O , X , X , O , O , O , O , O , X , O , X , O , O , O , X , O , X , O , X , O , O , X , O , X , O , X , X , X , X , X , O , O , O , X , O , O , O , X , X , O , X , O , O , X , O , O , O , X , X , X , O , X , X , X , O , O , O , X , X , X , O , X , X , X , X , X , X , X , O , X , O , O , X , X , O , X , O , O , X , O , O , O , X , X , X , O , O , O , O , X , X , X , X , X , X , X , O , X , X , O , X , O , X , X , O , O , O , X , X , O , O , O , X , X , O , O , O , O , X , O , O , X , X , X , X , X , X , O , O , X , O , X , O , X , O , X , X , O , O , O , O , X , O , X , X , O , X , O , O , O , O , O , O , O , O , O , O , O , O , X , O , X , O , X , O , X , O , X , O , O , O , X , O , X , X , O , O , X , X , O , X , X , O , X , X , X , X , O , X , X , X , X , X , X , O , O , O , O , X , X , O , X , O , O , X , O , O , X ]
--

data Cell = 
        NotChecked
        |X
        |O deriving (Eq)  

type Board = [Cell]

oppositeCell X = O
oppositeCell O = X
oppositeCell _ = NotChecked

toString NotChecked = "_"
toString X = "X"
toString O = "O"

instance Show (Cell) where
    show = toString

-- instance {-# OVERLAPPING #-} Show [Cell] where
--     show = toStrBoard


generateBoard :: Int -> Board
generateBoard length = [NotChecked | x <- [1..length]]


--Board to string
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

yielder index cell len = if index `mod` len == (len - 1) then (toString cell) ++ "\n" else (toString cell)

toStrBoard boardL = " " ++ (intercalate " " [yielder (fst x) (snd x) (boardHowManyRows boardL) | x <- zip [0..(length boardL)] boardL])
--

readCell :: [Char] -> Cell
readCell "X" = X
readCell "x" = X
readCell "O" = O
readCell "o" = O
readCell "_" = NotChecked
readCell x = NotChecked

boardHowManyRows board = isqrt (length board)

slice from to xs = take (to - from + 1) (drop from xs)

getNthRow n board = slice (n*(boardHowManyRows board)) ((n+1)*(boardHowManyRows board) - 1) board

getNthCol nth board = [snd(x) | x <- zip [0..(length board)] board, (fst(x) `mod` (boardHowManyRows board)) == nth]

-- howManyXsInRow row_num board = length (filter (==X) (getNthRow row_num board))
howManySymbolsInRow row_num board symb = length (filter (==symb) (getNthRow row_num board))
howManySymbolsInCol col_num board symb = length (filter (==symb) (getNthCol col_num board))

diffs [] = []
diffs ls = zipWith (-) (tail ls) ls

indicesInRow row_num board symb = elemIndices (symb) (getNthRow row_num board)
indicesInCol col_num board symb = elemIndices (symb) (getNthCol col_num board)

distancesInRow row_num board symb = sum ([x | x <- diffs (indicesInRow row_num board symb)])
distancesInCol col_num board symb = sum ([x | x <- diffs (indicesInCol col_num board symb)])

evalRowOld row_num board symb = (howManySymbolsInRow row_num board symb)^3 - (distancesInRow row_num board symb)^2
evalColOld col_num board symb = (howManySymbolsInCol col_num board symb)^3 - (distancesInCol col_num board symb)^2

evalRowsOld board symb = sum [evalRowOld index board symb | index <- [0..(boardHowManyRows board)]]
evalColsOld board symb = sum [evalColOld index board symb | index <- [0..(boardHowManyRows board)]]

evalBoardOld board symb = (evalRowsOld board symb) + (evalColsOld board symb)


indicesDiffsRow row_num board symb = diffs (indicesInRow row_num board symb)
getSymbGroupsRow row_num board symb = splitOneOf [2..19] (indicesDiffsRow row_num board symb)
getGroupsLengthsRow row_num board symb = [length x + 1 | x <- (getSymbGroupsRow row_num board symb)]
evalRow row_num board symb = sum [evalChain x | x <- getGroupsLengthsRow row_num board symb]


indicesDiffsCol col_num board symb = diffs (indicesInCol col_num board symb)
getSymbGroupsCol col_num board symb = splitOneOf [2..19] (indicesDiffsCol col_num board symb)
getGroupsLengthsCol col_num board symb = [length x + 1 | x <- (getSymbGroupsCol col_num board symb)]
evalCol col_num board symb = sum [evalChain x | x <- getGroupsLengthsCol col_num board symb]


evalChain 2 = 1.0
evalChain 3 = 5.0
evalChain 4 = 100.0
evalChain x = 0


get2dArrayFromList board = chunksOf (boardHowManyRows board) board

getDiagonals board = diagonals (get2dArrayFromList board)

getDiagonalsCw board = diagonals (cwRotate (get2dArrayFromList board))

cwRotate = map reverse . transpose


getGroupsDiagonals board symb = [splitOneOf [2..19] (diffs (elemIndices (symb) x)) | x <- getDiagonals board]
getGroupsLengthsDiagonals board symb = [length x + 1 | x <- concat $ getGroupsDiagonals board symb]
evalDiagonals board symb = sum [evalChain x | x <- getGroupsLengthsDiagonals board symb]


getGroupsDiagonalsCw board symb = [splitOneOf [2..19] (diffs (elemIndices (symb) x)) | x <- getDiagonalsCw board]
getGroupsLengthsDiagonalsCw board symb = [length x + 1 | x <- concat $ getGroupsDiagonalsCw board symb]
evalDiagonalsCw board symb = sum [evalChain x | x <- getGroupsLengthsDiagonalsCw board symb]



evalRows board symb = sum [evalRow index board symb | index <- [0..(boardHowManyRows board)]]
evalCols board symb = sum [evalCol index board symb | index <- [0..(boardHowManyRows board)]]

evalBoard board symb = (evalRows board symb) + (evalCols board symb) + (evalDiagonals board symb) + (evalDiagonalsCw board symb)

-- IO

getUserChoice symb = do
    putStrLn ((toString symb) ++ " | Type in row: ")
    row <- getLine
    return (if (((read row :: Int) >= 0) && ((read row :: Int) <= 19)) then (read row :: Int) else 0)

main = do  
    row <- getUserChoice X
    putStrLn (show row)

findAvailableMoves board = [(quot x (boardHowManyRows board), mod x (boardHowManyRows board)) | x <- elemIndices NotChecked board]

generateAvailableBoards whoseMove currState = [replaceElem ((fst x)*(boardHowManyRows currState) + (snd x)) whoseMove currState | x <- findAvailableMoves currState]

generateGameTree whoseMove initState = Node (evalBoard initState (oppositeCell whoseMove), evalBoard initState whoseMove, initState) [Node (evalBoard x (oppositeCell whoseMove), evalBoard x whoseMove, x) [] | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeTwoDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTree (oppositeCell whoseMove) x | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeThreeDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTreeTwoDepths (oppositeCell whoseMove) x | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeFourDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTreeThreeDepths (oppositeCell whoseMove) x | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeFiveDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTreeFourDepths (oppositeCell whoseMove) x | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeSixDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTreeFiveDepths (oppositeCell whoseMove) x | x <- (generateAvailableBoards whoseMove initState)]

generateGameTreeDepths whoseMove initState 1 = Node (evalBoard initState (oppositeCell whoseMove), evalBoard initState whoseMove, initState) [Node (evalBoard x (oppositeCell whoseMove), evalBoard x whoseMove, x) [] | x <- (generateAvailableBoards whoseMove initState)]
generateGameTreeDepths whoseMove initState depth = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameTreeDepths (oppositeCell whoseMove) x (depth - 1)| x <- (generateAvailableBoards whoseMove initState)]
-- generateGameTreeFourDepths whoseMove initState = Node (evalBoard initState whoseMove,  evalBoard initState (oppositeCell whoseMove), initState) [generateGameThreeDepths whoseMove x | x <- (generateAvailableBoards whoseMove initState)]