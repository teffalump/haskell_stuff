-- first haskell shit

tripleMe x = x + x + x

tripleUs x y z = x*x + y*y + z*z

tripleSmall x = if x > 100
                    then x
                    else tripleMe x

first x = take x

square x = x*x
sumSquares x = sum (take x [square y | y <- [0,1..]])

evenDiv7 x = take x [ y | y <- [0,1..], mod y 7 == 0]

rightTriangles x y = [ (a,b,c) | c <- [1..y], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == x]

pythagTriplets x = [ (a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

sqrtFloor x = floor (sqrt x)

factors' x = [3,5..sqrtFloor x]

factorial :: Integer -> Integer
factorial x = product [1..x]

-- simple, stupid, and slow
fib :: (Integral a) => a -> a
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n - 2)


-- scanl? hmm...looks great
fib' :: Int -> Int
fib' n = fibs !! n
        where fibs = 0 : scanl (+) 1 fibs

months :: Integer -> String
months 1 = "January"
months 2 = "February"
months 3 = "March"
months 4 = "April"
months 5 = "May"
months 6 = "June"
months 7 = "July"
months 8 = "August"
months 9 = "September"
months 10 = "October"
months 11 = "November"
months 12 = "December"
months x = "Don't smoke crack"

dotProduct :: (Num a) => (a,a) -> (a,a) -> a
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

dropIndex :: Int -> [a] -> [a]
dropIndex n l = take n l ++ reverse (take ( length l - n -1 ) (reverse l) )

-- This function rotates a list, forward and backward (+/- n, respectively)
-- this is much, much faster for longer lists when the shifts are close to the
-- length of the list in either direction
rotate :: Int -> [a] -> [a]
rotate index list
    | index == (-1) = [last list] ++ init list
    | index == 1 = tail list ++ [head list]
    | iterations >= half_length && index < 0 = rotate (length_l - iterations - 1) ( rotate (1) list )
    | iterations >= half_length && index > 0 =  rotate (-(length_l - iterations -1)) (rotate (-1) list)
    | otherwise = rotate (iterations - 1) (rotate 1 list)
    where iterations = mod index (length list)
          half_length = (length list) `div` (2)
          length_l = length list
--
-- This function rotates a list, forward and backward (+/- n, respectively)
-- simple and slow
rotate' :: Int -> [a] -> [a]
rotate' 0 l = l
rotate' 1 (x:xs) = xs ++ [x]
rotate' n l = rotate' (mod n (length l) - 1 ) ( rotate' 1 l ) 

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
                        smaller = quicksort [b | b <- xs, b <= x]
                        greater = quicksort [a | a <- xs, a > x]
                   in smaller ++ [x] ++ greater

-- check if prime -- WIP
isPrime :: Integral a => a -> Bool
isPrime x = if null [ y | y <- [2..floor (sqrt (fromIntegral x))], mod x y == 0]
                    then True
                    else False

--stupid prime generator
genPrimes' :: Integral a => a -> [a]
genPrimes' x = [ y | y <- [2..x], isPrime y ]

-- gives factors of x
-- simple, stupid, and slow
divisors :: Integral a => a -> [a]
divisors x = [ y | y <- [1..x `div` 2], mod x y == 0]

-- filter list for item, removing every instance of item
-- more narrow filter implementation
filter' :: Eq a => a -> [a] -> [a]
filter' _ [] = []
filter' c (h:t) = if h == c 
                    then filter' c t
                    else h:(filter' c t)
-- find factors
{-factors :: Int -> [Int]-}
{-factors x = [ y | y <- [2..sqrt x], mod x y == 0]-}

{-remainders x = [ mod x y | y <- (factors x)]-}
{-remainders x = map (mod x) (factors x)-}

{-primeTest x = if product (remainders x) /= 0-}
                {-then True-}
                {-else False-}

{-sievePrime x = take x (2:[ y | y <- [3,5..], primeTest y])-}
