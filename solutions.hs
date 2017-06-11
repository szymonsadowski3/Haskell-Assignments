import Data.Char

-- 1. Implementacja (.) (5 pkt)
dot f g x = f (g x)

-- 2. Potroíc kȧzdy wyraz w líscie (chodzi o [1,2] = [1,1,1,2,2,2]a nie mnozenie)
tripling list = concat [replicate 3 x | x <- list]

--3. Pobrác wiersz od u̇zytkownika i wypisác ilóśc słów
wordsCnt = do
    row <- getLine
    return (length (words row))

-- 4. Monada dla Problem a = Ok a | Error String (15 pkt)
data Problem a = Ok a | Error String

instance Functor Problem where
    fmap f (Ok a) = Ok (f a)

instance Applicative Problem where
    pure a = Ok a
    (<*>) (Ok f) (Ok a) = Ok (f a)

instance Monad Problem where
    (>>=) (Ok a) f = f a
    return a = Ok a

-- 2. Funkcja która zwraca stringa bez białych znaków
noWhitespaces str = filter (/=' ') str

-- 3. Napisác map za pomoc ̨a foldl i konstruktorów list
mapp func [] = []
mapp func list = reverse (foldl (\x y -> ((func y):x)) [] list)

-- 4. Funkcja, która odczyta ze standardowego wej́scia liczb ̨e,i zwróci sum ̨e jej cyfr
digitSum = do
    row <- getLine
    return (sum [(ord x) - (ord '0') | x <- row])

-- 1. napisác map za pomoc ̨a foldr
mappp func [] = []
mappp func list = foldr (\x y -> ((func x):y)) [] list
