import Data.Char (toUpper)

-- 1.2.1
r = 5.0

-- 1.2.2
x = 5 -- x is 5.

y = 6 -- y is 6.

answer =
  2
    * {-
      block comment
      -} 4 {- inline comment -}
    * 7

-- 1.2.3
-- Broken
-- r = 3.0

b = a * 2

a = 3

-- 1.2.4
area r = pi * r ^ 2

-- 1.2.6
areaTriangle b h = (b * h) / 2

-- 1.2.7
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

-- 1.3.3
myAbs x
  | x < 0 = 0 - x
  | otherwise = x

-- 1.7.1
biggerOne a b = if a > b then a else b

-- 1.7.2
pts 1 = 10
pts 2 = 6
pts 3 = 4
pts _ = 0

-- 1.7.3
myFst :: (a, b) -> a
myFst (x, _) = x

myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "Empty List"

myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = error "Empty List"

-- 1.7.4
badRoots a b c = ((- b + sqrt (b * b - 4 * a * c)) / (2 * a), (- b - sqrt (b * b - 4 * a * c)) / (2 * a))

goodRoots a b c =
  let sdisc = sqrt (b * b - 4 * a * c)
      twice_a = 2 * a
   in ((- b + sdisc) / twice_a, (- b - sdisc) / twice_a)

-- 1.8.1
f x = x + 3

square x = x ^ 2

-- 1.9.2
main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")

doGuessing num = do
  putStr "Enter your guess: "
  guess <- getLine
  if read guess < num
    then do
      putStrLn "Too low!"
      doGuessing num
    else
      if read guess > num
        then do
          putStrLn "Too high!"
          doGuessing num
        else putStrLn "You Win!"

-- 1.9.3
-- Broken
-- main = do
--  putStrLn "What is your name?"
--  putStrLn ("Hello " ++ getLine)

-- Broken
makeLoud = map toUpper

--
-- main = do
--  loudName <- makeLoud name
--  putStrLn ("Hello " ++ loudName ++ "!")

mainLoudName = do
  name <- getLine
  let loudName = makeLoud name
  putStrLn ("Hello " ++ loudName ++ "!")
