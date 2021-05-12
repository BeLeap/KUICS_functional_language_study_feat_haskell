myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

concatList :: [a] -> [a] -> [a]
concatList [] ys = ys
concatList (x : xs) ys = x : (concatList xs ys)
