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
