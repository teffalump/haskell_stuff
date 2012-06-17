-- A list of really useful number theory-like functions
module NumTheory (
        sieveTo,
        fibs,
        isPrime,
        mOrd,
        primeFactors,
        primeFactors',
        factors,
        primes) 

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
isPrime x = if (x > 1) && not (any (\y -> (x `mod` y) == 0) [2..floor (sqrt (fromIntegral x))])
                then True
                else False

-- prime factors of a number
p_factor :: [Integer] -> Integer -> [Integer]
p_factor _ 1 = []
p_factor [] y = [y]
p_factor (x:xs) y = if y `mod` x == 0
                    then
                        x:(p_factor (x:xs) (y `div` x))
                    else
                        p_factor (xs) y

primeFactors' y = p_factor (sieveTo (toInteger . floor . sqrt . fromIntegral $ y)) y

-- other way to get primeFactors and primes
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

-- factors of x
factors :: Integer -> [Integer]
factors =  nub . map (product) . init . subsequences . primeFactors

-- multiplicative order of 10 mod x
mOrd :: Integer -> Maybe Integer
mOrd p = find (\x -> if (10^x `mod` p) == 1 then True else False ) $ [1..p-1] 
