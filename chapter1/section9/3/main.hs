import Data.Char (toUpper)

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
