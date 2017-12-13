lengthOfPath s = div (abs a + abs b) 2
   where
      countHexes :: [String] -> Int -> Int -> (Int, Int)
      countHexes [] ns ew = (ns, ew)
      countHexes ("n":rest) ns ew = countHexes rest (ns+2) ew
      countHexes ("s":rest) ns ew = countHexes rest (ns-2) ew
      countHexes ("e":rest) ns ew = countHexes rest ns (ew+2)
      countHexes ("w":rest) ns ew = countHexes rest ns (ew+2)
      countHexes ("nw":rest) ns ew = countHexes rest (ns+1) (ew-1)
      countHexes ("sw":rest) ns ew = countHexes rest (ns-1) (ew-1)
      countHexes ("ne":rest) ns ew = countHexes rest (ns+1) (ew+1)
      countHexes ("se":rest) ns ew = countHexes rest (ns-1) (ew+1)
      (a, b) = countHexes s 0 0

parseInput [] = []
parseInput ('n':'w':rest) = "nw" : parseInput rest
parseInput ('n':'e':rest) = "ne" : parseInput rest
parseInput ('s':'w':rest) = "sw" : parseInput rest
parseInput ('s':'e':rest) = "se" : parseInput rest
parseInput ('w':rest)     =  "w" : parseInput rest
parseInput ('e':rest)     =  "e" : parseInput rest
parseInput ('n':rest)     =  "n" : parseInput rest
parseInput ('s':rest)     =  "s" : parseInput rest
parseInput (_:rest)       =        parseInput rest

main = do
   rawInput <- getContents
   let input = parseInput rawInput
   -- part 1
   print $ lengthOfPath $ input
   -- part 2
   print $ maximum $ map lengthOfPath [take n input | n <- [1..length input]]
