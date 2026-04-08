-- Int / Integer

sumCool :: Int -> Int -> Int
sumCool a b = a + b

sumFive :: Int -> Int
sumFive = sumCool 5

-- uncurry

-- Пара! (Int, Int)
-- (,)

sum2 :: (Int, Int) -> Int
sum2 = uncurry sumCool

min' :: Integer -> Integer -> Integer
min' x y
    | x < y = x
    | otherwise = y

-- True, False :: 

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

maj3 :: Bool -> Bool -> Bool -> Bool
maj3 True True _ = True
maj3 True _ True = True
maj3 _ True True = True
maj3 _ _ _ = False

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

step :: (Integer, Integer) -> Integer -> (Integer, Integer)
step pair 0 = pair
step (a, b) n = step (b, a+b) (n-1)

fib' :: Integer -> Integer
fib' x = snd $ step (0, 1) x

fib'' :: Integer -> Integer
fib'' x = snd $ step' (0, 1) x
    where
        step' pair 0 = pair
        step' (a, b) n = step (b, a+b) (n-1)

-- Int, Tuple, Bool, a -> b
data IntList = Empty | Cons Integer IntList

type Qunatity = Integer
newtype TotalyNotAnInteger = TNAInt Integer

data Cat = Cat Integer Integer Integer

catAge :: Cat -> Integer
catAge (Cat a _ _) = a

data Cat' = Cat'
    { age :: Integer
    , children :: Integer
    , lives :: Integer
    }

tail' :: IntList -> IntList
tail' Empty = Empty
tail' (Cons _ t) = t

len' :: IntList -> Integer
len' Empty = 0
len' (Cons _ t) = 1 + len' t

concat' :: IntList -> IntList -> IntList
concat' Empty x = x
concat' (Cons h t) x = Cons h $ concat' t x

-- List
-- a => [a] -- cправа название типа
-- [], (h : t)
-- f [a, b, c] = b

nat :: [Integer]
nat = 0 : map (1 +) nat

nat' :: [Integer]
nat' = [0,1..]

squares :: [Integer]
squares = [n*n | n <- nat, n > 200]

primes :: [Integer]
primes = filterPrime [2, 3 ..]
    where filterPrime (p : xs) = p : filterPrime [x | x <- xs,  x `mod` p /= 0]

data BinIntTree = Leaf Int | Parent Int BinIntTree BinIntTree

example :: BinIntTree
example = Parent 1 (Parent 3 (Leaf 3) (Leaf 5)) (Leaf 4)

sumTree :: BinIntTree -> Int
sumTree (Leaf i) = i
sumTree (Parent i l r) = i + sumTree l + sumTree r

data Expression = Var 
                | Const Integer
                | Sum Expression Expression
                | Mult Expression Expression
                | Sin Expression
                | Cos Expression

showExp :: Expression -> String -- [Char]
showExp Var = "x"
showExp (Const i) = show i -- про Show мы поговорим отдельно
showExp (Sum a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Mult a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Sin a) = "sin(" ++ showExp a ++ ")"
showExp (Cos a) = "cos(" ++ showExp a ++ ")"

derivative :: Expression -> Expression
derivative Var = Const 1
derivative (Const _ ) = Const 0
derivative (Sum a b) = Sum (derivative a) (derivative b)
derivative (Mult a b) = Sum (Mult a $ derivative b) (Mult (derivative a) b)
derivative (Sin a) = Mult (Cos a) (derivative a)
derivative (Cos a) = Mult (Mult (Const $ -1) (Sin a)) (derivative a)

exex :: Expression
exex = Mult (Sin (Var)) (Sum (Cos Var) (Mult (Const 5) Var))

-- curry :: ((a, b) -> c) -> a -> b -> c
-- ($), snd, fst
-- *, * -> * [a]

ex1 :: [Int]
ex1 = [2, 3, 4]

ex2 :: [String]
ex2 = ["aaa", "bbb"]

data BinTree a = Leaf' a | Parent' a (BinTree a) (BinTree a)

sumTree' :: Num a => BinTree a -> a
sumTree' (Leaf' i) = i
sumTree' (Parent' i l r) = i + sumTree' l + sumTree' r

merge :: Ord a => [a] -> [a] -> [a]
merge [] r = r
merge l [] = l
merge l@(lh:lt) r@(rh:rt)
    | lh < rh = lh : merge lt r
    | otherwise = rh : merge l rt

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort a) (mergeSort b) where
    (a, b) = halve xs