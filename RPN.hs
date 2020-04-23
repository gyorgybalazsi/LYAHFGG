import Data.Char

solveRPN = head . foldl push [] . words
  where
    push (x:y:ys) "+" = (x + y):ys 
    push (x:y:ys) "*" = (x * y):ys
    push (x:y:ys) "-" = (x - y):ys
    push xs x = read x : xs
    