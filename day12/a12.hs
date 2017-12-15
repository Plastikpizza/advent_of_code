import qualified Data.Set as Set

parseConnection line = map readInt $ words reducedLine
   where
      reducedLine = [x | x <- line, elem x "0987654321 "]
      readInt :: String -> Int
      readInt = read

insertAll [] s = s
insertAll (l:r) s = insertAll r (Set.insert l s)

findGroup :: Set.Set Int -> [[Int]] -> Set.Set Int
findGroup s [] = s
findGroup s (a:b)
   | hasMember s a = findGroup (insertAll a s) b
   | otherwise = findGroup s b
   where
      hasMember i j = elem True [Set.member e i | e <- j]

findWholeGroup s l = helper (Set.fromList []) s l l
   where
      helper p c [] r = if p == c then c else helper c c r r
      helper p c l r = helper p (findGroup c l) [] r

main = do
   rawInput <- getContents
   let input = map parseConnection $ lines rawInput
   print $ Set.size $ findWholeGroup (Set.singleton 0) input
   print $ Set.size $ Set.fromList [findWholeGroup (Set.singleton (head i)) input | i <- input]
