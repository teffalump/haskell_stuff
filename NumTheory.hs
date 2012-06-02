-- A list of really useful number theory-like functions
module NumTheory (
        sieveTo,
        fibs,
        isPrime,
        mOrd,
        primeFactor,
        factors) 

where 

--List minus for ordered increasing lists; [1..4] `minus` [2,3] ==> [1,4]
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus xs _ = xs

--prime sieve
sieveTo :: Int -> [Int]
sieveTo m = 2: sieve [3,5..m]
    where
        sieve [] = []
        sieve (x:xs) = x : sieve (xs `minus` [x*x,x*x+2*x..m])

-- very fast fib
fibs :: [Int] 
fibs = 0 : scanl (+) 1 fibs

-- check if prime -- WIP
isPrime :: Integral a => a -> Bool
isPrime x = if null [ y | y <- [2..floor (sqrt (fromIntegral x))], mod x y == 0]
                    then True
                    else False

-- prime factors of a number
p_factor :: [Int] -> Int -> [Int]
p_factor _ 1 = []
p_factor [] y = [y]
p_factor all@(x:xs) y = if y `mod` x == 0
                    then
                        x:(p_factor all (y `div` x))
                    else
                        p_factor (xs) y

primeFactor y = p_factor . sieveTo . floor . sqrt . fromIntegral

-- factors of x
factors :: Int -> [Int]
factors =  nub . map (product) . init . subsequences . p_factors'

-- multiplicative order
mOrd :: Integer -> Integer
mOrd p = head . filter (\x -> if (10^x `mod` p) == 1 then True else False ) $ [1..p-1] 
