import System.Random
import Control.Monad
-- import Control.Monad.State implemented below

-- Randomness w/o State monad

threeCoins' :: StdGen -> (Bool, Bool, Bool)  
threeCoins' gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin) 


-- threeCoins' $ mkStdGen 100

-- State monad

newtype State s a = State { runState :: s -> (a,s) }  

instance Functor (State s) where 
    fmap f (State g) = State $ \s0 -> let (a, s1) = g s0
                                      in (f a, s1)

instance Applicative (State s) where 
    pure x = State (x,)
    (<*>) = ap

instance Monad (State s) where  
    return x = State (x,) 
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState 

                                    
-- Stack pup and push

type Stack = [Int]  

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
    
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs) 

get = State $ \s -> (s,s) 
put newState = State $ const ((),newState)  

-- Randomness w/ State monad

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c) 

-- runState threeCoins $ mkStdGen 100

-- Monadic functions

