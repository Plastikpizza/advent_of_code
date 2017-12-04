import Data.List(nub, sort)

isValidPhrase input = input == nub input

isValidPhrase_v2 [] = True
isValidPhrase_v2 (w1:rest)
   | thereIsAnAnagramOf w1 = False
   | otherwise = isValidPhrase_v2 rest
   where
      thereIsAnAnagramOf word = elem True (map (areAnagrams word) rest)
      areAnagrams a b = sort a == sort b

main = do
   raw_input <- getContents
   let input = lines raw_input
   -- part 1
   print $ length $ filter (==True) $ map (isValidPhrase . words) input
   -- part 2
   print $ length $ filter (==True) $ map (isValidPhrase_v2 . words) input
