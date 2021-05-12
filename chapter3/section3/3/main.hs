pairOff :: Int -> Either String Int
pairOff people
  | people < 0 = Left "Can't pair off negative number of people"
  | people > 30 = Left "Too many people for this activity"
  | even people = Right (people `div` 2)
  | otherwise = Left "Can't pair off an odd number of people"

groupPeople :: Int -> String
groupPeople people = case pairOff people of
  Right groups -> "We have " ++ show groups ++ " group(s)."
  Left problem -> "Problem! " ++ problem
