badDoubleList :: [Integer] -> [Integer]
badDoubleList [] = []
badDoubleList (n : ns) = (2 * n) : badDoubleList ns

badTripleList :: [Integer] -> [Integer]
badTripleList [] = []
badTripleList (n : ns) = (3 * n) : badTripleList ns

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n : ns) = (m * n) : multiplyList m ns
