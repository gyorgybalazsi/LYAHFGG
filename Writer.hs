-- https://nestedsoftware.com/2018/12/04/book-notes-learn-you-a-haskell-for-great-good-2cnp.64983.html
import Control.Monad

newtype Writer w a = Writer { runWriter :: (a, w) }  deriving (Show, Eq)

instance (Monoid w) => Functor (Writer w) where 
    fmap f (Writer (x, log)) = Writer (f x, log)

instance (Monoid w) => Applicative (Writer w) where 
     pure x = Writer (x, mempty)
     (<*>) = ap

instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x 
                           in Writer (y, v `mappend` v') 
    
tell :: (Monoid w) => w -> Writer w ()
tell v = Writer ((), v)

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
    
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5 
    tell ["multplying two inputs"]
    return (a*b) 
    
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)


keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  

-- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  