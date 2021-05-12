addStr :: Float -> String -> Float
addStr x str = x + read str

sumStr :: [String] -> Float
sumStr = foldl addStr 0.0

sumStrLet :: [String] -> Float
sumStrLet =
  let addStrInLet x str = x + read str
   in foldl addStrInLet 0.0

sumStrWhere :: [String] -> Float
sumStrWhere = foldl addStrInWhere 0.0
  where
    addStrInWhere x str = x + read str

doStuff :: Int -> String
doStuff x
  | x < 3 = report "less than three"
  | otherwise = report "normal"
  where
    report y = "the input is " ++ y
