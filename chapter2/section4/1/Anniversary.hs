module Anniversary (Anniversary (..)) where

data Anniversary
  = Birthday String Int Int Int -- name, year, month, day
  | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" 1968 7 3

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" 1987 3 4
