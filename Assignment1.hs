module Assignment1 (subst, interleave, unroll) where

subst :: Eq t=>t->t->[t]->[t]
subst _ _ [] = []
subst a b (x:xs)
  | a==x       =b:subst a b xs
  | otherwise  =x:subst a b xs

interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave [] (y:ys) = y: interleave [] ys
interleave (x:xs)[] = x: interleave xs []
interleave (x:xs)(y:ys) = x : y : interleave xs ys

unroll :: Int -> [a] -> [a]
unroll _ [] = []
unroll a (x:xs) = take a (cycle(x:xs))


