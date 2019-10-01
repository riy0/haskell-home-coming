quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
	larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
