myFst :: (a, b) -> a
myFst (x, _) = x

myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "Empty List"

myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = error "Empty List"
