contrivedMap :: ([a] -> a -> b) -> [a] -> [b]
contrivedMap f [] = []
contrivedMap f list@(x : xs) = f list x : contrivedMap f xs

badContrivedMap :: ([a] -> a -> b) -> [a] -> [b]
badContrivedMap f [] = []
badContrivedMap f (x : xs) = f (x : xs) x : badContrivedMap f xs

data Foo2 = Bar2 | Baz2 {bazNumber :: Int, bazName :: String}

h :: Foo2 -> Int
h Baz2 {bazName = name} = length name
h Bar2 {} = 0

data Foo3 = Bar3 | Baz3 Int

g2 :: Foo3 -> Bool
g2 Bar3 {} = True
g2 Baz3 {} = False
