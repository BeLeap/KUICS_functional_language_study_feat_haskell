mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f [x] = x
myFoldr1 f (x : xs) = f x (foldr1 f xs)
myFoldr1 _ [] = error "myFoldr1: empty list"

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x : xs) = foldl f x xs
myFoldl1 _ [] = error "myFoldl1: empty list"
