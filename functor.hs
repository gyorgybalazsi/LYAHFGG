import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  


{- main = do line <- fmap (++"!") getLine
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"  -}