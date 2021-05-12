myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

multiplyListUsingMap :: Integer -> [Integer] -> [Integer]
multiplyListUsingMap m = map ((*) m)
