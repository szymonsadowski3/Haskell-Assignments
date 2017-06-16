module MyTree where

    data MTree a = Node a ([MTree a]) | Empty deriving (Show, Eq)

    children (Node a tail) = length tail

    getNodes (Empty) = []
    getNodes (Node v tail) = [(sel2 v)]++ (concat [getNodes x | x <- tail])


    -- testTree = genTree 2 3 1

    -- genTree :: Int -> Int -> a -> MTree a
    -- genTree 0 _ _ = Empty
    -- genTree depth branches val = Node val ([genTree (depth-1) branches val | x <- [1..branches]])

    -- genLeaf elem = (Node elem [])
    sel2 (a1, a2, a3) = a2

    instance Functor (MTree) where
        fmap _ Empty = Empty
        fmap f (Node v branches) = Node (f v) ([fmap f x | x <- branches])