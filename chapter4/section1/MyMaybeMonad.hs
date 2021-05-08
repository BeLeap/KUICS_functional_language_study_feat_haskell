data MyMaybe a = MyJust a | MyNothing

return :: a -> MyMaybe a
return = MyJust

(>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
m >>= g = case m of
  MyNothing -> MyNothing
  MyJust x -> g x
