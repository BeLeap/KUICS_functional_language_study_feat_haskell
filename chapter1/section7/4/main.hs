badRoots a b c = ((- b + sqrt (b * b - 4 * a * c)) / (2 * a), (- b - sqrt (b * b - 4 * a * c)) / (2 * a))

goodRoots a b c =
  let sdisc = sqrt (b * b - 4 * a * c)
      twice_a = 2 * a
   in ((- b + sdisc) / twice_a, (- b - sdisc) / twice_a)
