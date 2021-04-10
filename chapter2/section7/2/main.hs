sumStrLambda :: [String] -> Float
sumStrLambda = foldl (\x str -> x + read str) 0.0

tailLambda = (\(_ : xs) -> xs)
