badRetainEven :: [Int] -> [Int]
badRetainEven [] = []
badRetainEven (n : ns) =
  if (mod n 2) == 0
    then n : badRetainEven ns
    else badRetainEven ns

retainEven = filter even

anotherRetainEven es = [n | n <- es, even n]

retainLargeEvens es = [n | n <- es, even n, n > 100]

evensMinusOne es = [n - 1 | n <- es, even n]

badFirstForEvenSeconds ps = [fst p | p <- ps, even (snd p)]

firstForEvenSeconds ps = [x | (x, y) <- ps, even y]

allPairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8]]

somePairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8], x + y > 8]
