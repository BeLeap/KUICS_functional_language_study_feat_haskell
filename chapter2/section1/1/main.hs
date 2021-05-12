factorial 0 = 1
factorial n = n * factorial (n - 1)

loopFactorial n = go n 1
  where
    go n res
      | n > 1 = go (n - 1) (res * n)
      | otherwise = res

mult n 0 = 0
mult n 1 = n
mult n m = (mult n (m - 1)) + n
