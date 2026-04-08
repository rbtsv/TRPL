data Nat = Z | S Nat

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat n
  | n < 0     = error "Nat cannot be negative"
  | n == 0    = Z
  | otherwise = S (integerToNat (n - 1))

instance Show Nat where
  show = show . natToInteger

instance Eq Nat where
  Z   == Z   = True
  S a == S b = a == b
  _   == _   = False

instance Ord Nat where
  compare Z     Z     = EQ
  compare Z     _     = LT
  compare _     Z     = GT
  compare (S a) (S b) = compare a b

add :: Nat -> Nat -> Nat
add Z     b = b
add (S a) b = S (add a b)

mul :: Nat -> Nat -> Nat
mul Z     _ = Z
mul (S a) b = add b (mul a b)

instance Num Nat where
  (+) = add
  (*) = mul
  abs = id
  signum Z = Z
  signum _ = S Z
  fromInteger = integerToNat

  negate _ = error "negate is not defined for Nat"
  (-) a b =
    case sub a b of
      Just r  -> r
      Nothing -> error "negative result in Nat subtraction"


sub :: Nat -> Nat -> Maybe Nat
sub a     Z     = Just a
sub Z     (S _) = Nothing
sub (S a) (S b) = sub a b


instance Enum Nat where
  toEnum n
    | n < 0     = error "Nat cannot be negative"
    | otherwise = integerToNat (toInteger n)

  fromEnum = fromInteger . natToInteger


instance Real Nat where
  toRational = toRational . natToInteger 

quotRemNat :: Nat -> Nat -> (Nat, Nat)
quotRemNat _ Z = error "division by zero"
quotRemNat a b = go a Z
  where
    go n q =
      case sub n b of
        Just r  -> go r (S q)
        Nothing -> (q, n)         


instance Integral Nat where
  toInteger = natToInteger
  quotRem = quotRemNat
  divMod = quotRemNat


main :: IO ()
main = do
  let a = 7 :: Nat
      b = 3 :: Nat
  putStrLn $ "a        = " ++ show a
  putStrLn $ "b        = " ++ show b
  putStrLn $ "a + b    = " ++ show (a + b)
  putStrLn $ "a - b    = " ++ show (a - b)
  putStrLn $ "a * b    = " ++ show (a * b)
  putStrLn $ "a `div` b = " ++ show (a `div` b)
  putStrLn $ "a `mod` b = " ++ show (a `mod` b)
  putStrLn $ "a == b   = " ++ show (a == b)
  putStrLn $ "a > b    = " ++ show (a > b)
  putStrLn $ "sub a b  = " ++ show (sub a b)
  putStrLn $ "sub b a  = " ++ show (sub b a)