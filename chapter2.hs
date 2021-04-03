-- 2.1.1

factorial 0 = 1
factorial n = n * factorial (n - 1)

loopFactorial n = go n 1
  where
    go n res
      | n > 1 = go (n - 1) (res * n)
      | otherwise = res

mult n 0 = 0
mult n 1 = n
mult n m = (mult n (m - 1)) + n

-- 2.1.2

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

concatList :: [a] -> [a] -> [a]
concatList [] ys = ys
concatList (x : xs) ys = x : (concatList xs ys)

-- 2.1.3

libraryFactorial n = product [1 .. n]

-- 2.2.1

badDoubleList :: [Integer] -> [Integer]
badDoubleList [] = []
badDoubleList (n : ns) = (2 * n) : badDoubleList ns

badTripleList :: [Integer] -> [Integer]
badTripleList [] = []
badTripleList (n : ns) = (3 * n) : badTripleList ns

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n : ns) = (m * n) : multiplyList m ns

-- 2.2.2

doubleList :: [Integer] -> [Integer]
doubleList xs = multiplyList 2 xs

betterDoubleList :: [Integer] -> [Integer]
betterDoubleList = multiplyList 2

applyToInteger :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToInteger _ [] = []
applyToInteger f (n : ns) = (f n) : (applyToInteger f ns)

betterMultiplyList :: Integer -> [Integer] -> [Integer]
betterMultiplyList m = applyToInteger ((*) m)

-- 2.2.3

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = (f x) : myMap f xs

multiplyListUsingMap :: Integer -> [Integer] -> [Integer]
multiplyListUsingMap m = map ((*) m)

-- 2.2.4

intsFrom n = n : intsFrom (n + 1)

positiveInts = intsFrom 1

-- 2.3.1

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f [x] = x
myFoldr1 f (x : xs) = f x (foldr1 f xs)
myFoldr1 _ [] = error "myFoldr1: empty list"

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x : xs) = foldl f x xs
myFoldl1 _ [] = error "myFoldl1: empty list"

-- 2.3.2

echoesFoldr :: [Int] -> [Int]
echoesFoldr = foldr (\x xs -> (replicate x x) ++ xs) []

echoesFoldl :: [Int] -> [Int]
echoesFoldl = foldl (\xs x -> xs ++ (replicate x x)) []

mapFold f = foldr (\x xs -> f x : xs) []

-- 2.3.4

badRetainEven :: [Int] -> [Int]
badRetainEven [] = []
badRetainEven (n : ns) =
  if ((mod n 2) == 0)
    then n : (badRetainEven ns)
    else badRetainEven ns

retainEven = filter even

anotherRetainEven es = [n | n <- es, even n]

retainLargeEvens es = [n | n <- es, even n, n > 100]

evensMinusOne es = [n - 1 | n <- es, even n]

badFirstForEvenSeconds ps = [fst p | p <- ps, even (snd p)]

firstForEvenSeconds ps = [x | (x, y) <- ps, even y]

allPairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8]]

somePairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8], x + y > 8]

-- 2.4.1

data Anniversary
  = Birthday String Int Int Int -- name, year, month, day
  | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" 1968 7 3

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" 1987 3 4

-- 2.4.2

badShowDate :: Int -> Int -> Int -> String
badShowDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
  name ++ " born " ++ badShowDate year month day
showAnniversary (Wedding name1 name2 year month day) =
  name1 ++ " married " ++ name2 ++ " on " ++ badShowDate year month day

type Name = String

type AnniversaryBook = [Anniversary]

-- 2.5.2

-- Broken
-- dropThree ([x,y,z] ++ xs) = xs

data Foo = Bar | Baz Int

f :: Foo -> Int
f Bar = 1
f (Baz x) = x - 1

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

-- 2.5.3

dropThree :: [a] -> [a]
dropThree (_ : _ : _ : xs) = xs
dropThree _ = []

-- 2.5.4

fstPlusSnd :: (Num a) => (a, a) -> a
fstPlusSnd (x, y) = x + y

norm3D :: (Floating a) => (a, a, a) -> a
norm3D (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- 2.5.5

f1 :: Int -> Int
f1 0 = 1
f1 1 = 5
f1 2 = 2
f1 _ = 1

g1 :: [Int] -> Bool
g1 (0 : []) = False
g1 (0 : xs) = True
g1 _ = False

-- 2.5.6

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

-- 2.5.7

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

-- 2.6.1

describeLetterIf :: Char -> String
describeLetterIf c =
  if c >= 'a' && c <= 'z'
    then "Lower case"
    else
      if c >= 'A' && c <= 'Z'
        then "Upper case"
        else "Not an ASCII letter"

describeLetterGuard :: Char -> String
describeLetterGuard c
  | c >= 'a' && c <= 'z' = "Lower case"
  | c >= 'A' && c <= 'Z' = "Upper case"
  | otherwise = "Not an ASCII letter"

g3 x y = (if x == 0 then 1 else sin x / x) * y

-- 2.6.2

fPiece 0 = 18
fPiece 1 = 15
fPiece 2 = 12
fPiece x = 12 - x

fCase x =
  case x of
    0 -> 18
    1 -> 15
    2 -> 12
    _ -> 12 - x

data Color = Black | White | RGB Int Int Int

describeBlackOrWhite :: Color -> String
describeBlackOrWhite c =
  "This color is "
    ++ case c of
      Black -> "black"
      White -> "white"
      RGB 0 0 0 -> "black"
      RGB 255 255 255 -> "white"
      _ -> "... uh... something else"
    ++ ", yeah?"

-- 2.6.3

doGuessing num = do
  putStr "Enter your guess: "
  guess <- getLine
  case compare (read guess) num of
    LT -> do
      putStrLn "Too low!"
      doGuessing num
    GT -> do
      putStrLn "Too high!"
      doGuessing num
    EQ -> do putStrLn "You Win!"

-- 2.7.1

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

-- 2.7.2

sumStrLambda :: [String] -> Float
sumStrLambda = foldl (\x str -> x + read str) 0.0

tailLambda = (\(_ : xs) -> xs)

-- 2.7.3

(!=) a b = not (a == b)

-- 2.7.4

multiplyListSection :: Integer -> [Integer] -> [Integer]
multiplyListSection m = map (m *)

myElem :: (Eq a) => a -> [a] -> Bool
x `myElem` xs = any (== x) xs

-- 2.8.1

quickSort :: (Ord a) => [a] -> [a]
-- Base case
quickSort [] = []
-- Recursion case
quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
  where
    less = filter (< x) xs
    equal = filter (== x) xs
    more = filter (> x) xs
