data BOIL = O | I deriving Show

bAnd I I = I
bAnd _ _ = O

data MyList a
    = Nil 
    | Cons a (MyList a)
    deriving Show

myHead (Cons x _) = x    
myHead Nil = error "empty list"

myTail (Cons _ xs) = xs
myTail Nil = error "empty list"

myMap _ Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

myNat = Cons 0 (myMap (+1) myNat)