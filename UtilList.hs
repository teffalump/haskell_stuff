module UtilList (
        minus,
        make_lists,
        cMap)

where
import Data.List
import Data.Traversable

--List minus for ordered increasing lists; [1..4] `minus` [2,3] ==> [1,4]
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus xs _ = xs

-- cool way to generate possible combinations of length n given list xs
make_lists n xs = nub . sequenceA . replicate n $ xs

-- utility func, map f over xs and concat xs to result
cMap :: (a -> a) -> [a] -> [a]
cMap _ [] = []
cMap f (x:xs) = (f x):x:(cMap f xs)
