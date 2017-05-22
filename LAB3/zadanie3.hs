testlist = [1..5]
boollist = [True, False, True]
boollist2 = [True, True, True]
boollist3 = [False, False, False]

mySum myType = foldl (\x y -> x + y) 0 myType

myProduct myType = foldl (\x y -> x * y) 1 myType

myReverse myType = foldl (\acc x -> x:acc) [] myType

myAnd myType = foldl (\x y -> (x && y)) True myType

myOr myType = foldl (\x y -> (x || y)) False myType

myHead myType = foldl (\acc x -> acc) [] myType

