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
