import Control.Applicative (Alternative(empty))
class Monoid m => Group m where
    inverse :: m -> m

instance Group Bool where
    inverse :: Bool -> Bool
    inverse True = False
    inverse False = True
instance Monoid Bool where
    mempty :: Bool
    mempty = False
instance Semigroup Bool where
    (<>) :: Bool -> Bool -> Bool
    False <> False = False
    True <> True = False
    _ <> _ = True

data BinTree a = Leaf a | Parent a (BinTree a) (BinTree a) deriving (Show, Ord)

instance Eq a => Eq (BinTree a) where
    (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (Leaf a) == (Leaf b) = a == b
    (Parent n x y) == (Parent m a b) = x == a && y == b && n == m
    _ == _ = False

-- fmap :: (a -> b) -> f a -> f b

instance Functor BinTree where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Parent n a b) = Parent (f n) (fmap f a) (fmap f b)

square :: (Functor f, Num b) => f b -> f b
square = fmap (\x -> x * x)

ex1 :: [Integer]
ex1 = square [5, 6, 7]
ex2 :: BinTree Integer
ex2 = (Parent 5 (Leaf 2) (Parent 5 (Leaf 4) (Leaf 3)))

-- foldable
-- foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable BinTree where
    foldr :: (a -> b -> b) -> b -> BinTree a -> b
    foldr f s (Leaf x) = f x s
    foldr f s (Parent n a b) = foldr f accum_right_sum a where
        right_sum = foldr f s b
        accum_right_sum = f n right_sum

-- Аппликативный функтор
-- (<*>) :: f (a -> b) -> f a -> f b
-- pure :: a -> f a

-- Монады : Functor f => Applicative f => Monad f
-- (>>=) :: m a -> (a -> m b) -> m b (join . fmap)

data Maybe' a = Just' a | Nothing'
instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap f (Just' a) = Just' $ f a
    fmap _ Nothing' = Nothing'
instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'
    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    f <*> Nothing' = Nothing'
    Nothing' <*> a = Nothing'
    (Just' f) <*> (Just' a) = Just' $ f a
instance Monad Maybe' where
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b 
    Nothing' >>= f = Nothing'
    Just' a >>= f = f a

type Birds = Integer
type Pole = (Birds, Birds)

-- Either l r = Left l | Right r 
-- Error a = Either String a

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs (left + n - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (right + n - left) < 4 = Just (left, right + n)
    | otherwise = Nothing

ex3 :: Maybe Pole
ex3 = return (0, 0) >>= landRight 2 >>= landLeft 100 >>= landRight 2

ex4 :: Maybe Pole
ex4 = do
    let start = (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

-- []
ex5 :: [(Integer, Char)]
ex5 = do
    x <- [1..10] --- [Int]
    y <- ['A'..'Z']
    return (x, y)

ex6 = [1..10] >>= (\x -> ['A'..'Z'] >>= (\y -> return (x, y))) 

-- >>= :: [a] -> (a -> [b]) -> [b]
-- [[b]]

newtype State s a = State { runState :: s -> (a, s) }
-- * -> *
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))
instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State sf) <*> (State sa) = State (\s0 ->
        let (fn, s1) = sf s0
            (a, s2) = sa s1 in (fn a, s2))
instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    State act >>= k = State $ \s -> let (a, s') = act s in runState (k a) s'

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

tos :: State Stack Int
tos = State $ \(x:xs) -> (x, x:xs)
 
stackManip :: State Stack Int
stackManip = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a + b)
    tos

ex140 :: (Int, Stack)
ex140 = runState stackManip []





