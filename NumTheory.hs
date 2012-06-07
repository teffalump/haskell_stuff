-- A list of really useful number theory-like functions
module NumTheory (
        sieveTo,
        fibs,
        isPrime,
        mOrd,
        primeFactors,
        factors) 

where 

import Data.List
import Data.Char

--List minus for ordered increasing lists; [1..4] `minus` [2,3] ==> [1,4]
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus xs _ = xs

--prime sieve
sieveTo :: Integer -> [Integer]
sieveTo m = 2: sieve [3,5..m]
    where
        sieve [] = []
        sieve (x:xs) = x : sieve (xs `minus` [x*x,x*x+2*x..m])

-- very fast fib
fibs :: [Integer] 
fibs = 0 : scanl (+) 1 fibs

-- check if prime -- WIP
isPrime :: Integral a => a -> Bool
isPrime x = if (x > 0) && (all (\y -> (x `mod` y) == 0) [2..floor (sqrt (fromIntegral x))])
                then True
                else False

-- prime factors of a number
p_factor :: [Integer] -> Integer -> [Integer]
p_factor _ 1 = []
p_factor [] y = [y]
p_factor all@(x:xs) y = if y `mod` x == 0
                    then
                        x:(p_factor all (y `div` x))
                    else
                        p_factor (xs) y

primeFactors y = p_factor (sieveTo (toInteger . floor . sqrt . fromIntegral $ y)) y

-- factors of x
factors :: Integer -> [Integer]
factors =  nub . map (product) . init . subsequences . primeFactors

-- multiplicative order of 10 mod x
mOrd :: Integer -> Maybe Integer
mOrd p = if (p `mod` 2 /= 0) && (p `mod` 5 /= 0)
            then find (\x -> if (10^x `mod` p) == 1 then True else False ) $ [1..p-1] 
            else Nothing

                        
