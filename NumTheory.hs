-- A list of really useful number theory-like functions
module NumTheory (
        sieveTo,
        fibs,
        isPrime,
        mOrd,
        primeFactors,
        primeFactors',
        factors,
        primes,
        toBin,
        power,
        fact) 

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
isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = False
    | x == 2 = True
    | otherwise = all ((/=0) . mod x) . sieveTo . floor . sqrt . fromIntegral $ x

-- prime factors of a number
p_factor :: [Integer] -> Integer -> [Integer]
p_factor _ 1 = []
p_factor [] y = [y]
p_factor (x:xs) y = if y `mod` x == 0
                    then
                        x:(p_factor (x:xs) (y `div` x))
                    else
                        p_factor (xs) y

primeFactors' y = p_factor (sieveTo . toInteger . floor . sqrt . fromIntegral $ y) y

-- other way to get primeFactors and primes
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors = factor primes
  where
    factor (p:ps) n
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (p:ps) (n `div` p)
        | otherwise      = factor ps n

-- factors of x
factors :: Integer -> [Integer]
factors =  nub . map (product) . init . subsequences . primeFactors

-- multiplicative order of 10 mod x
mOrd :: Integer -> Maybe Integer
mOrd p = find (\x -> if (10^x `mod` p) == 1 then True else False ) $ [1..p-1] 

-- Binary conversion stuff
power :: Integer -> Integer -> Integer
power x y = x^y

-- factorial
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact x = product . enumFromTo 2 $ x

--places needed
places p = reverse . takeWhile ((<=p) . power 2) $ [0..]

-- recursion, use foldl? Seems better but failed when tried
{-test' :: Integer -> String -> [Integer] -> Integer-}
{-test' i b [] = read b :: Integer-}
{-test' i b (x:xs) = if m <= i-}
                    {-then test' (i - m) (b ++ "1") (xs) -}
                    {-else test' i (b ++ "0") (xs)-}
                        {-where m = 2 `power` x-}

{-toBin' :: Integer -> Integer-}
{-toBin' p = test' p "0" (places p)-}

--convert to binary from decimal
-- foldl use
toBin :: Integer -> Integer
toBin n = read (snd . foldl (\(rem,dig) x -> if 2 `power` x <= rem
                            then (rem - 2 `power` x, dig ++ "1")
                            else (rem,dig ++ "0")) (n, "0")
                            . places $ n) :: Integer

