import Control.Monad.State
import Data.Char 
import Control.Monad
import System.IO

type Stack = [Int]

pop :: State Stack Int 
pop = state $ \(x:xs) -> (x,xs)
  
push :: Int -> State Stack () 
push a = state $ \xs -> ((),a:xs)

stackManips1 :: State Stack ()
stackManips1 = do
    pop
    pop
    push 3

stackManips2 :: State Stack ()
stackManips2 = do
    a <- pop
    if rem a 2 == 0 then push (a+1) else do
        push a
        push (a+2)

maybeHalf :: Int -> Maybe Int
maybeHalf n 
    | rem n 2 == 0 = Just (div n 2)
    | otherwise = Nothing
                

{- maybeDemo :: Int -> Int -> Int -> Maybe [Int]
maybeDemo n1 n2 n3 = do
    n1' <- maybeHalf n1
    n2' <- maybeHalf n2
    n3' <- maybeHalf n3
    return [n1',n2',n3'] -}

maybeDemo :: [Int] -> Maybe [Int]
maybeDemo xs = do
    sequence $ map maybeHalf xs

{- main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!" -}

 
  
{- main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" -}  


{- main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words -}

{- main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  -}


{- main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main   -}

{- main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  -}

{- main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors -}

{- main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        getLine)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  -} 

main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle

