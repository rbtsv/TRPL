mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where (ys, zs) = splitInHalfs xs

splitInHalfs  [] = ([],[])
splitInHalfs [x] = ([x], [])
splitInHalfs (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = splitInHalfs zs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
    | (x <= y) = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)


    


