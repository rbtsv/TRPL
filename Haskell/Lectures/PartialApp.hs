f' x y z = x + y + z 
--f'' :: Num a => a -> a -> a-> a
f'' :: Num a => a -> a -> a-> a
f'' = \x y z -> x + y + z 
f =  \x -> (\y -> (\z -> x + y + z))

g = f 2

h' x z = f' x 0 z 
h x  = f' x 0 
