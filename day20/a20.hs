import Data.List (elemIndex)
import Data.Maybe
data Vector = Vector{x::Int, y::Int, z::Int} deriving (Show, Eq)
data Particle = Particle {p::Vector, v::Vector, a::Vector} deriving (Show, Eq)

plus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

replace a b [] = []
replace a b (i:l) = if i == a then b : replace a b l else i : replace a b l

parseLine :: String -> [Int]
parseLine line = map read $ words [x | x <- (replace ',' ' ' line), elem x "-0987654321 "]

particeFromList [x1,y1,z1,x2,y2,z2,x3,y3,z3] = Particle p v a
   where
      p = Vector x1 y1 z1
      v = Vector x2 y2 z2
      a = Vector x3 y3 z3

move particle = particle{p=np, v=nv}
   where
      nv = plus (a particle)  (v particle)
      np = plus nv (p particle)

moveAll = map move

maybeMove particle
   | isNothing particle = Nothing
   | otherwise = Just (move (fromJust particle))

maybeMoveAll = map maybeMove

moveAllTimes list 0 = list
moveAllTimes list n = moveAllTimes (moveAll list) (n-1)

manhattanDistance particle = abs (x i) + abs (y i) + abs (z i)
   where
      i = p particle

collide a [] = a
collide Nothing _ = Nothing
collide (Just particle) (mH:mList)
   | isNothing mH = collide (Just particle) mList
   | (p particle) == (p (fromJust mH)) && different = Nothing
   | otherwise = collide (Just particle) mList
   where
      different = fromJust mH /= particle

collideAll list = map (flip collide list) list

partTwo 0 list = length $ filter isJust list
partTwo n list = partTwo (n-1) (collideAll (maybeMoveAll list))

main = do
   rawInput <- getContents
   let particles = (map (particeFromList . parseLine) (lines rawInput))
   let moved = moveAllTimes particles 1000
   print $ elemIndex (minimum $ map manhattanDistance moved) (map manhattanDistance moved)
   let maybeParticles = map (Just) particles
   print $ partTwo 1000 maybeParticles
