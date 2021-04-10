multiplyListSection :: Integer -> [Integer] -> [Integer]
multiplyListSection m = map (m *)

myElem :: (Eq a) => a -> [a] -> Bool
x `myElem` xs = any (== x) xs
