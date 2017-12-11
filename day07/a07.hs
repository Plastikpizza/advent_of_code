import Debug.Trace (trace)
import Data.List (nub)
data Program = Program {name::String, weight::Int, children::[String]} deriving (Show, Eq)

parseProgram string = Program n w c
   where
      toWords = words string
      n = head toWords
      w = read $ tail $ init $ toWords !! 1
      c = if length toWords > 2
          then tail $ dropWhile (/= "->") (toWords)
          else []

parsePrograms = map parseProgram

accumulateWeight p l
   | (length $ nub w) > 1 = trace ("found something in node " ++ name p ++ ". diff: " ++ d ++ ".\nw: " ++ (show w)) ((weight p)+sum w)
   | otherwise = (weight p) + sum w
   where
      w = [accumulateWeight i l | i <- l, elem (name i) (children p)]
      diff list = maximum list - minimum list
      d = show $ diff w

findParent :: Program -> [Program] -> Maybe Program
findParent program (p:list)
   | elem (name program) (children p) = Just p
   | list == [] = Nothing
   | otherwise = findParent program list

main = do
   input <- getContents
   let programs = parsePrograms (lines input)
   -- part 1
   print $ head $ [name p | p <- programs, (findParent p programs) == Nothing]
   -- part 2
   let root = head $ filter (\a -> name a == "vvsvez") programs
   print $ accumulateWeight root programs
