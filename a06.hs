import Debug.Trace(trace)

-- TODO use Data.Array to make it fast and beautiful!

set n v l = [if i==n then v else l!!i | i <-[0..(length l)-1]]

setMaxZero (a:l) = setMaxZero' [] a l
   where
      setMaxZero' left n right
         | right == [] || n >= maximum right = left ++ (0 : right)
         | otherwise          = setMaxZero' (left++[n]) (head right) (tail right)

distribute 0 start banks = banks
distribute n start banks = distribute (n-1) nextIndex nextBanks
   where
      nextIndex = mod (start+1) (length banks)
      nextBanks = set start ((banks!!start) +1) banks

redistribute l = distribute ml mi nb
   where
      ml = maximum l
      mi = mod ((head [i | i <- [0..(length l)-1], l!!i == ml])+1) (length l)
      nb = setMaxZero l

partOne :: [Int] -> [[Int]] -> Int
partOne l h
   | elem l h  = length h
   | otherwise = partOne (redistribute l) (l:h)
