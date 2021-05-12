data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance (Show a) => Show (Tree a) where
  show (Leaf x) = show x
  show (Branch left right) = "{" ++ show left ++ " | " ++ show right ++ "}"
