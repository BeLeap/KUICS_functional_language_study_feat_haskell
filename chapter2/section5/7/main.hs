y1 =
  let (x : _) = map (* 2) [1, 2, 3]
   in x + 5

y2 = x + 5
  where
    (x : _) = map (* 2) [1, 2, 3]

swap = \(x, y) -> (y, x)

data MyMaybe a = MyNothing | MyJust a

myCatMaybes :: [MyMaybe a] -> [a]
myCatMaybes ms = [x | MyJust x <- ms]

putFirstChar = do
  (c : _) <- getLine
  putStrLn [c]
