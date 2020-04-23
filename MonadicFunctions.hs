import Control.Monad
import Data.List
import Data.Ratio
import Data.Function

powerset :: [a] -> [[a]]  
powerset = filterM $ const [True, False]

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

keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  



-- Safe RPN calculator

solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result  

foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = fmap (:xs) (readMaybe numberString)  -- fmap instead of liftMő

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  

-- The Prob monad

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  
    -- fmap f (Prob xs) = Prob $ liftM2 (,) (f . fst) snd

instance Applicative Prob where
    pure x = Prob [(x,1%1)] 
    (<*>) = ap


flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concatMap multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  
    -- where multAll (Prob innerxs,p) = map (liftM2 (,) fst ((*r) . snd)) innerxs 

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob [] 

data Coin = Heads | Tails deriving (Show, Eq)  

coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
    
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  

flipTwice = do
    a <- coin  
    b <- coin   
    return (all (==Tails) [a,b])
  
flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c]) 

aggregatedFilpTwice = map (liftM2 (,) (fst . head) (sum . map snd)) $ groupBy ((==) `on` fst) $  getProb flipTwice

aggregatedFilpThree = map (liftM2 (,) (fst . head) (sum . map snd)) $ groupBy ((==) `on` fst) $  getProb flipThree