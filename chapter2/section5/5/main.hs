f1 :: Int -> Int
f1 0 = 1
f1 1 = 5
f1 2 = 2
f1 _ = 1

g1 :: [Int] -> Bool
g1 [0] = False
g1 (0 : xs) = True
g1 _ = False
