import qualified Data.Map as Map
import Data.List(intersperse)

data Direction = North|South|West|East deriving (Show, Eq)
data Flag = Infected|Weakened|Flagged deriving (Show, Eq)
data Cell = Cell {flag::Flag, coords::(Int,Int)} deriving (Show, Eq)
data Carrier = Carrier {facing::Direction,
                        position::(Int,Int),
                        flags::Map.Map (Int, Int) Flag,
                        caused::Int} deriving (Show, Eq)

carrierFromInput input = Carrier North (0,0) (Map.fromList [((i-12, j-12), Infected)| i <- [0..24], j<-[0..24], ((input!!j)!!i) == '#']) 0

turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight North = East
turnRight West  = North
turnRight South = West
turnRight East  = South

turn180Dg North = South
turn180Dg South = North
turn180Dg East  = West
turn180Dg West  = East

turn c = case s of
   Nothing -> c{flags=Map.insert (position c) Weakened (flags c),facing=(turnLeft.facing)c}
   Just f -> case f of
      Infected -> c{flags=Map.insert (position c) Flagged remCur, facing=(turnRight.facing)c}
      Weakened -> c{flags=Map.insert (position c) Infected remCur,caused=caused c+1}
      Flagged  -> c{flags=remCur, facing=(turn180Dg.facing)c}
   where
      s = Map.lookup (position c) (flags c)
      remCur = Map.delete (position c) (flags c)

move c = case facing c of
   North -> c{position=(a,b+1)}
   South -> c{position=(a,b-1)}
   East  -> c{position=(a+1,b)}
   West  -> c{position=(a-1,b)}
   where
      (a, b)=position c

step = move.turn

steps 0 c = c
steps n c = steps (n-1) (step c)

main = do
   rawInput <- getContents
   let input = lines rawInput
   let c = carrierFromInput $ reverse input
   (print . caused . steps 10000000) c
