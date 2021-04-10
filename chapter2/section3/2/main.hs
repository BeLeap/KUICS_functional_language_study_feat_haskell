echoesFoldr :: [Int] -> [Int]
echoesFoldr = foldr (\x xs -> replicate x x ++ xs) []

echoesFoldl :: [Int] -> [Int]
echoesFoldl = foldl (\xs x -> xs ++ replicate x x) []

mapFold f = foldr (\x xs -> f x : xs) []
