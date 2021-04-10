quickSortHighOrder :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
--Base case
quickSortHighOrder _ [] = []
-- Recursion case
quickSortHighOrder compare (x : xs) =
  quickSortHighOrder compare less
    ++ (x : equal)
    ++ quickSortHighOrder compare more
  where
    less = filter (\y -> y `compare` x == LT) xs
    equal = filter (\y -> y `compare` x == EQ) xs
    more = filter (\y -> y `compare` x == GT) xs
