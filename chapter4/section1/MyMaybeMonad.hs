data MyMaybe a = MyJust a | MyNothing deriving (Show)

myReturn :: a -> MyMaybe a
myReturn = MyJust

(>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
m >>= g = case m of
  MyNothing -> MyNothing
  MyJust x -> g x
