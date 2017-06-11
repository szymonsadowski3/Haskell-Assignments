data Box a = Box a | Empty deriving Show

instance Functor Box where
    fmap f (Box a) = Box (f a)

instance Applicative Box where
    pure a = Box a
    (<*>) (Box f) (Box a) = Box (f a)

instance Monad Box where
    (>>=) (Box a) f = f a
    return a = Box a


data Box2 a = Box2 a a | Empty2 deriving Show

instance Functor Box2 where
    fmap f (Box2 v1 v2) = Box2 (f v1) (f v2)

instance Applicative Box2 where
    pure a = Box2 a a 
    (<*>) (Box2 f1 f2) (Box2 v1 v2) = Box2 (f1 v1) (f2 v2) 

instance Monad Box2 where
    (>>=) (Box2 v1 v2) f = f v1
    return a = Box2 a a 

