import Data.List
import Data.List.Split
import Data.Universe.Helpers
import Data.Ord
import Debug.Trace

data Cell = 
        NotChecked
        |X
        |O deriving (Eq)  

toString NotChecked = "_"
toString X = "X"
toString O = "O"


instance Show (Cell) where
    show = toString

readCell :: [Char] -> Cell
readCell "X" = X
readCell "x" = X
readCell "O" = O
readCell "o" = O
readCell "_" = NotChecked
readCell x = NotChecked


data Board = Board
    { rowsNumber    :: Integer
    , board2d       :: [[Cell]]
    }

elo = Board 2 [[X,X], [O,O]]
soBlank = Board 19 [[X,X,O,X,X,X,O,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,O,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X], [O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O], [X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X]]

dispRow row = intercalate " " [show x | x <- row]

toStrBoard (Board rowsNum board) = intercalate "\n" [dispRow x | x <- board]

--UTILS
cwRotate = map reverse . transpose

diffs [] = []
diffs ls = zipWith (-) (tail ls) ls
--

--EVALUATION FUNCTION
evalChain 2 = 1.0
evalChain 3 = 5.0
evalChain 4 = 100.0
evalChain 5 = 5000.0
evalChain x = 0

indicesInRows (Board rowsNum board) symb = [elemIndices (symb) x | x <- board]
indicesDiffsInRows boardCls symb = [diffs x | x <- indicesInRows boardCls symb]

getSymbGroups boardCls symb = concat [splitOneOf [2..19] x | x <- (indicesDiffsInRows boardCls symb)]
getChainsRowWise boardCls symb = [(length x) + 1 | x <- (getSymbGroups boardCls symb)]

--DIAGONAL EVAL
getDiagonals (Board rowsNum board) = diagonals board


evalBoardRowWise boardCls symb = sum $ fmap evalChain $ getChainsRowWise boardCls symb
evalBoard (Board rowsNum board) symb = (evalBoardRowWise (Board rowsNum board) symb) + (evalBoardRowWise (Board rowsNum (cwRotate board)) symb)

-- evalRowWise boardCls symb
