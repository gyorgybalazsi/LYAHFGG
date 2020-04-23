optPow :: Int -> Int -> Maybe Int
optPow x y 
  | y == 0 = Just 1
  | y > 0 = let Just z = optPow x (y - 1) in Just (y * z)
  | otherwise = Nothing