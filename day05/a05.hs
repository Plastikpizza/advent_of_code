import Debug.Trace (trace)

jumpPartOne (left, n, right)
   | n > 0 = (left ++ [n+1] ++ (take (n-1) right), head (drop (n-1) right), drop n right)
   | n < 0 = (dropBack n1 left, last (dropBack (n1-1) left), (takeBack (n1-1) left) ++ [(n+1)] ++ right)
   | otherwise = (left, n+1, right)
   where
      n1 = abs n

jumpPartTwo (left, n, right)
   | n > 0 = (left ++ [nv] ++ (take (n-1) right), head (drop (n-1) right), drop n right)
   | n < 0 = (dropBack n1 left, last (dropBack (n1-1) left), (takeBack (n1-1) left) ++ [nv] ++ right)
   | otherwise = (left, n+1, right)
   where
      n1 = abs n
      nv = if n > 2 then n-1 else n+1

dropBack n l = (reverse . drop n . reverse) l
takeBack n l = (reverse . take n . reverse) l

countJumpsPartOne (a, b, c) = countJumpsPartOne' 0 (a, b, c)
   where
      countJumpsPartOne' c (left, n, right)
         | rl < n = c+1
         | n < -ll = c+1
         | otherwise = countJumpsPartOne' (c+1) (jumpPartOne (left, n, right))
         where
            rl = length right
            ll = length left

readInt :: String -> Int
readInt = read

countJumpsPartTwo (a, b, c) = countJumpsPartTwo' 0 (a, b, c)
   where
      countJumpsPartTwo' c (left, n, right)
         | rl < n = c+1
         | n < -ll = c+1
         | otherwise = countJumpsPartTwo' (c+1) (jumpPartTwo (left, n, right))
         where
            rl = length right
            ll = length left

main = do
   raw_input <- getContents
   let input = lines raw_input
   let numbs = map readInt input
   print $ countJumpsPartOne ([],head numbs,tail numbs)
   print $ countJumpsPartTwo ([],head numbs,tail numbs)
