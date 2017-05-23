{-# LANGUAGE FlexibleInstances #-}

import Data.List

--data
testB = generateBoard 361
filledB = [O , NotChecked , NotChecked , O , NotChecked , NotChecked , NotChecked , O , NotChecked , O , NotChecked , X , O , X , X , NotChecked , O , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , O , O , X , NotChecked , O , X , NotChecked , O , O , X , O , X , X , NotChecked , X , X , NotChecked , O , X , X , NotChecked , NotChecked , O , NotChecked , NotChecked , X , NotChecked , X , NotChecked , O , O , NotChecked , NotChecked , O , O , O , O , X , NotChecked , O , NotChecked , NotChecked , NotChecked , X , NotChecked , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , O , O , NotChecked , X , X , O , X , X , X , O , NotChecked , NotChecked , NotChecked , NotChecked , X , NotChecked , X , O , NotChecked , NotChecked , NotChecked , X , X , X , NotChecked , O , X , NotChecked , O , O , X , O , O , O , NotChecked , O , O , X , NotChecked , X , O , X , X , O , O , NotChecked , X , NotChecked , O , NotChecked , NotChecked , X , NotChecked , NotChecked , X , NotChecked , X , X , O , X , X , O , O , NotChecked , NotChecked , NotChecked , NotChecked , O , NotChecked , NotChecked , O , NotChecked , O , X , X , X , NotChecked , X , O , O , X , NotChecked , NotChecked , NotChecked , O , O , X , O , O , X , NotChecked , NotChecked , NotChecked , NotChecked , X , X , NotChecked , NotChecked , O , X , O , X , X , O , O , NotChecked , NotChecked , X , O , X , NotChecked , X , X , NotChecked , X , O , NotChecked , NotChecked , X , X , O , NotChecked , O , NotChecked , NotChecked , X , X , O , NotChecked , O , X , O , X , O , O , NotChecked , X , X , O , X , NotChecked , O , O , O , X , NotChecked , X , O , NotChecked , X , NotChecked , NotChecked , NotChecked , NotChecked , NotChecked , X , NotChecked , X , O , X , O , O , O , NotChecked , NotChecked , O , O , NotChecked , NotChecked , X , NotChecked , O , NotChecked , NotChecked , O , NotChecked , O , X , O , NotChecked , NotChecked , NotChecked , NotChecked , O , O , X , O , NotChecked , X , NotChecked , X , O , X , X , NotChecked , O , NotChecked , X , NotChecked , NotChecked , NotChecked , X , O , O , X , NotChecked , X , NotChecked , NotChecked , NotChecked , O , NotChecked , O , NotChecked , X , O , NotChecked , NotChecked , O , NotChecked , X , NotChecked , O , X , NotChecked , O , NotChecked , O , O , X , O , O , NotChecked , NotChecked , O , O , X , O , X , X , X , O , O , O , O , NotChecked , X , X , O , NotChecked , O , X , O , X , X , X , NotChecked , O , NotChecked , X , X , X , X , O , O , O , X , X , NotChecked , NotChecked , X , X , O , O , NotChecked , NotChecked , O , NotChecked , NotChecked , NotChecked , NotChecked , X , O , X , X]
list = [1..9]
list3 = [O,X,NotChecked,X,O,NotChecked,NotChecked,X,O]
--

data Cell = 
        NotChecked
        |X
        |O deriving (Eq)  

type Board = [Cell]

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
readCell "O" = O
readCell "_" = NotChecked

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

evalRow row_num board symb = (howManySymbolsInRow row_num board symb)^3 - (distancesInRow row_num board symb)^2
evalCol col_num board symb = (howManySymbolsInCol col_num board symb)^3 - (distancesInCol col_num board symb)^2

evalRows board symb = sum [evalRow index board symb | index <- [0..(boardHowManyRows board)]]
evalCols board symb = sum [evalCol index board symb | index <- [0..(boardHowManyRows board)]]

evalBoard board symb = (evalRows board symb) + (evalCols board symb)

-- main = do  
--     putStrLn "Hello, type [X | O | _]: "  
--     cell <- getLine  
--     putStrLn ((readCell cell))  