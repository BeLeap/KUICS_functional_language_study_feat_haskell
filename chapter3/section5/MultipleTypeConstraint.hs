foo :: (Num a, Show a, Show b) => a -> a -> b -> String
foo x y t =
  show x ++ " plus " ++ show y ++ " is " ++ show (x + y) ++ ".  " ++ show t
