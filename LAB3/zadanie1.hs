data BinTree a = Node a (BinTree a) (BinTree a) | Empty


ontwothree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
ontwo = Node 1 (Empty) (Node 2 Empty Empty)
cmplxtree = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty)
morecmplx = Node 1 (Node 2 (Node 3 (Node 6 (Node 7 Empty Empty) Empty) Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty)


insert :: BinTree a -> a -> BinTree a
insert (Node v Empty Empty) ins = Node v (Node ins Empty Empty) Empty
insert (Node v left right) ins = Node v (insert left ins) right


empty :: BinTree a -> Bool
empty (Empty) = True
empty (Node _ _ _) = False


isBin :: BinTree a -> Bool
isBin (Empty) = True
isBin (Node _ _ _) = True


isInTree :: Eq a => BinTree a -> a -> Bool
isInTree Empty _ = False
isInTree (Node v left right) elem 
    | v == elem = True
    | otherwise = (isInTree left elem) || (isInTree right elem)


height Empty = 0
height (Node v left right) = (max (height left) (height right)) + 1


isBalanced :: BinTree a -> Bool
isBalanced Empty = True
isBalanced (Node v left right) = abs ((height left) - (height right)) < 2



traverseVLR Empty = []
traverseVLR (Node v left right) = v:((traverseVLR left)++(traverseVLR right))


traverseLVR Empty = []
traverseLVR (Node v left right) = (traverseLVR left)++(v:(traverseLVR right))


traverseLRV Empty = []
traverseLRV (Node v left right) = (traverseLRV left)++((traverseLRV right)++[v])


traverseVRL Empty = []
traverseVRL (Node v left right) = v:((traverseVRL right)++(traverseVRL left))


traverseRVL Empty = []
traverseRVL (Node v left right) = (traverseRVL right)++(v:(traverseRVL left))


traverseRLV Empty = []
traverseRLV (Node v left right) = (traverseRLV right)++((traverseRLV left)++[v])


toString :: Show a => (BinTree a) -> String
toString Empty = ""
toString (Node v left right) = (show v) ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"

leaves Empty = []
leaves (Node v Empty Empty) = [v]
leaves (Node v left right) = (leaves left)++(leaves right)


nnodes Empty = 0
nnodes (Node v Empty Empty) = 1
nnodes (Node v left right) = 1 + (nnodes left) + (nnodes right)


nsum Empty = 0
nsum (Node v Empty Empty) = v
nsum (Node v left right) = v + (nsum left) + (nsum right)


tmap :: BinTree a -> (a -> b) -> BinTree b
tmap Empty funct = Empty
tmap (Node v Empty Empty) funct = (Node (funct v) Empty Empty)
tmap (Node v left right) funct = (Node (funct v) (tmap left funct) (tmap right funct))


merge :: BinTree a -> BinTree a -> BinTree a
merge (Node v Empty Empty) (Node v2 left2 right2) = Node v (Node v2 left2 right2) Empty
merge (Node v left right) (Node v2 left2 right2) = Node v (merge left (Node v2 left2 right2)) right


class MyShow a where
    myShow :: a -> String


instance Show a => MyShow (BinTree a) where
    myShow = toString


data STree a = SEmpty | SLeaf a | SBranch a (STree a) (STree a) deriving Show

estree = SBranch 1 (SLeaf 2) (SLeaf 3)

convSTreeBin :: (BinTree a) -> (STree a)
convSTreeBin Empty = SEmpty
convSTreeBin (Node v Empty Empty) = SLeaf v
convSTreeBin (Node v left right) = SBranch v (convSTreeBin left) (convSTreeBin right) 




