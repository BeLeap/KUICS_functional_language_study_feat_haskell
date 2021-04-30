myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []

foo = flip map [1 .. 4]
