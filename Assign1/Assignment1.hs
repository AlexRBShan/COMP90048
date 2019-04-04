module Assignment1 (subst, interleave, unroll) where
-- subst implementation
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs) = if a == x
    then b:(subst a b xs)
    else x:(subst a b xs)
--interleave implementation
interleave :: [t] -> [t] -> [t]
interleave a [] = a
interleave [] b = b
interleave (x1:x1s) (x2:x2s) = x1:x2:(interleave x1s x2s)
--unroll implementation
len :: [t] -> Int
len [] = 0
len (_:xs) = 1 + len xs
unroll :: Int -> [a] -> [a]
unroll _ [] = []
unroll n (x:xs)
    | n <=0 = []
    | n >= len (x:xs) = (x:xs) ++ (unroll (n - len (x:xs)) (x:xs))
    | n > 0 && n < len (x:xs) =  x:(unroll (n-1) xs)
