import Data.List

data Cell = 
        NotChecked
        |X
        |O

type Board = [Cell]

toString NotChecked = "_"
toString X = "X"
toString O = "O"

instance Show (Cell) where
    show = toString

generateBoard :: Int -> Board
generateBoard length = [NotChecked | x <- [1..length]]


--Board to string
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

yielder index cell len = if index `mod` len == (len - 1) then (toString cell) ++ "\n" else (toString cell)

toStrBoard boardL = " " ++ (intercalate " " [yielder (fst x) (snd x) (isqrt (length boardL)) | x <- zip [0..(length boardL)] boardL])
--


testB = generateBoard 361
