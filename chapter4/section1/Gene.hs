data Person = Person {name :: String, age :: Int, father :: Person, mother :: Person}

instance Show Person where
  show Person {name = name, age = age} = name ++ " " ++ show age

empty = Person {name = "empty", age = 0, father = empty, mother = empty}

p1 = Person {name = "person1", age = 103, father = empty, mother = empty}

p2 = Person {name = "person2", age = 102, father = empty, mother = empty}

p3 = Person {name = "person3", age = 105, father = empty, mother = empty}

p4 = Person {name = "person4", age = 100, father = empty, mother = empty}

p5 = Person {name = "person5", age = 50, father = p1, mother = p2}

p6 = Person {name = "person6", age = 45, father = p3, mother = p4}

p7 = Person {name = "person7", age = 20, father = p5, mother = p6}

getFather :: Person -> Maybe Person
getFather Person {father = father} = Just father

getMother :: Person -> Maybe Person
getMother Person {mother = mother} = Just mother

badMaternalGrandfather :: Person -> Maybe Person
badMaternalGrandfather p =
  case getMother p of
    Nothing -> Nothing
    Just mom -> getFather mom

badBothGrandfather :: Person -> Maybe (Person, Person)
badBothGrandfather p = case getFather p of
  Nothing -> Nothing
  Just dad ->
    case getFather dad of
      Nothing -> Nothing
      Just gf1 ->
        case getMother p of
          Nothing -> Nothing
          Just mom ->
            case getFather mom of
              Nothing -> Nothing
              Just gf2 ->
                Just (gf1, gf2)

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p = getMother p >>= getFather

bothGrandfather :: Person -> Maybe (Person, Person)
bothGrandfather p =
  getFather p
    >>= ( \dad ->
            getFather dad
              >>= ( \gf1 ->
                      getMother p
                        >>= ( \mom ->
                                getFather mom
                                  >>= ( \gf2 -> return (gf1, gf2)
                                      )
                            )
                  )
        )

betterBothGrandfather :: Person -> Maybe (Person, Person)
betterBothGrandfather p = do
  dad <- getFather p
  gf1 <- getFather dad
  mom <- getMother p
  gf2 <- getFather mom
  return (gf1, gf2)