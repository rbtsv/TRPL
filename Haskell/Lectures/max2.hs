first' x y = x

max' x y 
    | (x >= y) = x
    | otherwise = y

max2 :: Ord a => [a] -> (a, a)
max2 (x:y:zs) = _max2 (max x y) (min x y) zs
                    where _max2 m1 m2 [] = (m1, m2)
                          _max2 m1 m2 (x:xs)
                            | x >= m1 = _max2 x m1 xs
                            | x >= m2 = _max2 m1 x xs
                            | otherwise = _max2 m1 m2 xs