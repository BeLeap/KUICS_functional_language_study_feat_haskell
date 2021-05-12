-- Broken
-- dropThree ([x,y,z] ++ xs) = xs

data Foo = Bar | Baz Int

f :: Foo -> Int
f Bar = 1
f (Baz x) = x - 1

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
