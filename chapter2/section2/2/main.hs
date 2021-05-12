multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n : ns) = (m * n) : multiplyList m ns

doubleList :: [Integer] -> [Integer]
doubleList xs = multiplyList 2 xs

betterDoubleList :: [Integer] -> [Integer]
betterDoubleList = multiplyList 2

applyToInteger :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToInteger _ [] = []
applyToInteger f (n : ns) = f n : applyToInteger f ns

betterMultiplyList :: Integer -> [Integer] -> [Integer]
betterMultiplyList m = applyToInteger ((*) m)
