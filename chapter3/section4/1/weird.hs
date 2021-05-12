data Weird a b
  = First a
  | Second b
  | Third [(a, b)]
  | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
  where
    g (First x) = First (fa x)
    g (Second y) = Second (fb y)
    g (Third z) = Third (map (\(x, y) -> (fa x, fb y)) z)
    g (Fourth w) = Fourth (g w)

weirdFold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
  where
    g (First x) = f1 x
    g (Second y) = f2 y
    g (Third z) = f3 z
    g (Fourth w) = f4 (g w)
