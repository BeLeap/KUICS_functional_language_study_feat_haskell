import Anniversary

badShowDate :: Int -> Int -> Int -> String
badShowDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
  name ++ " born " ++ badShowDate year month day
showAnniversary (Wedding name1 name2 year month day) =
  name1 ++ " married " ++ name2 ++ " on " ++ badShowDate year month day

type Name = String

type AnniversaryBook = [Anniversary]
