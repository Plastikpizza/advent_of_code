import Split

data Bridge = Bridge {pairs::[(Int, Int)], end::Int, pool::[(Int, Int)]}
   deriving (Eq, Show)

addPair bridge new@(a,b) = bridge{pairs = new : (pairs bridge), end = n, pool= filter (/= new) (pool bridge)}
   where
      n = if a == (end bridge) then b else a

parsePair line = (read f, read r)::(Int, Int) where
   f = head s
   r = last s
   s = splitOn "/" line

findCompatible bridge = [p | p <- allPairs, isCompatible bridge p]
   where
      allPairs = pool bridge
      isCompatible bridge (a, b) = (end bridge) == a || (end bridge) == b

rateBridge bridge = sum $ map pairSum (pairs bridge) where
   pairSum (a, b) = a + b

buildBridge bridge
   | compatibles == [] = [bridge]
   | otherwise = concat $ map buildBridge $ map (addPair bridge) compatibles
   where
      compatibles = (findCompatible bridge)

main = do
   input <- getContents
   let allPairs = map parsePair $ lines input
   let bridges = buildBridge (Bridge [] 0 allPairs)
   print $ maximum $ map rateBridge $ bridges
   let len = maximum $ map (length . pairs) bridges
   let briges1 = [b | b <- bridges, (length $ pairs b) == len]
   print $ maximum $ map rateBridge $ briges1
