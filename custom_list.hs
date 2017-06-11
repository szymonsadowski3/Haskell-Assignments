data LList a = Node a (LList a) | Empty deriving Show

testList = Node 1$Node 2$Node 3 Empty
testList2 = Node 4$Node 5$Node 6 Empty

mymap :: (a -> b) -> (LList a) -> (LList b)
mymap func (Node v tail) = (Node (func v) (mymap func tail))
mymap _ Empty = Empty

myfoldr :: (a -> b -> b) -> b -> (LList a) -> b
myfoldr func initval (Node v tail) = func v (myfoldr func initval tail)
myfoldr _ initval Empty = initval

myfoldl :: (b -> a -> b) -> b -> (LList a) -> b
myfoldl func initval (Node v tail) = myfoldl func (func initval v) tail
myfoldl _ initval Empty = initval

myzipwith :: (a -> b -> c) -> (LList a) -> (LList b) -> (LList c)
myzipwith func (Node v1 tail1) (Node v2 tail2) = Node (func v1 v2) (myzipwith func tail1 tail2)
myzipwith _ _ _ = Empty

plusplus :: (LList a) -> (LList a) -> (LList a)
plusplus (Node v1 tail1) (Node v2 tail2) = (Node v1 (plusplus tail1 (Node v2 tail2)))
plusplus Empty (Node v tail) = (Node v tail)

fromNormalList :: [a] -> (LList a)
fromNormalList (x:xs) = Node x (fromNormalList xs)
fromNormalList [] = Empty

toNormalList :: (LList a) -> [a]
toNormalList (Node v tail) = v:(toNormalList tail)
toNormalList Empty = []

instance Functor (LList) where
    fmap f (Node v tail) = Node (f v) (fmap f tail)
    fmap _ Empty = Empty

instance Applicative LList where
    pure a = Node a Empty
    (<*>) (Node f Empty) (Node a Empty) = (Node (f a) Empty)