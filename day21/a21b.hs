import qualified Data.Map as Map
import Data.List (intersperse, nub)
import Split (splitOn)
import Debug.Trace(trace)

data Rule = Rule {input::Pattern, output::Pattern} deriving (Eq, Show)
data Pattern = Pattern {size::Int, grid::Map.Map (Int, Int) Bool} deriving (Eq)

instance Show Pattern where
   show p = concat $ intersperse "\n" [gline y p | y <-[0..(size p)-1]] where
      gline y a = intersperse ' ' [if element x y p then '#' else '.'| x <- [0..(size p)-1]]

listFromPattern p = [line i p | i <- [0..(size p -1)]]

flatListFromPattern = concat . listFromPattern

element x y p = case Map.lookup (x, y) (grid p) of
   Nothing -> False
   otherwise -> True

line y p = [element x y p | x <- [0..(size p)-1]]

patternFromFlatList l = patternFromList $ groupList n l where
   n = (round . sqrt . fromIntegral . length) l
   groupList n [] = []
   groupList n l = (take n l) : groupList n (drop n l)

patternFromList l = Pattern theSize theMap where
   theMap = Map.fromList [((x, y), True) | x <- [0..(theSize-1)],
                                           y <- [0..(theSize-1)],
                                           l!!y!!x == True]
   theSize = (length l)

turn = patternFromList . twist 1 . listFromPattern where
   row n p = [i!!n | i <- p]

   twist 0 p = p
   twist 1 p = [row (abs (i-l)) p | i <- [0..l]] where
      l = length (p) -1
   twist n p = twist (n-1) (twist 1 p)

mirror = patternFromList . (map reverse) . listFromPattern

allTurns p = nub [p, p1, p2, p3] where
   p1 = turn p
   p2 = turn p1
   p3 = turn p2

allMutations p = nub $ a ++ map mirror a where
   a = allTurns p

ruleFromLine line = Rule (makePattern left) (makePattern right) where
   stepOne = splitOn " => " line
   makePattern = patternFromList . boolList
   boolList a = [[c=='#' | c<-b] | b<-a]
   left = splitOn "/" (head stepOne)
   right = splitOn "/" (last stepOne)

startPattern = Pattern 3 (Map.fromList [((1,0), True), ((2,1), True),
                                        ((0,2), True), ((1,2), True),
                                        ((2,2), True)])

match p r
   | elem p (allMutations (input r)) = output r
   | otherwise = p

matchAny p rs = head $ [i | i<-a, i /= p] where
   a = map (match p) rs

breakInPacks pattern
   | sizeMultipleOf2 = [buildTwoPack (2*j) (2*i) pattern | i <- [0..halfPatternSize-1], j <- [0..halfPatternSize-1]]
   | otherwise = [buildThreePack (3*j) (3*i) pattern | i <- [0..thirdPatternSize-1], j<-[0..thirdPatternSize-1]]
   where
      buildTwoPack a b p = patternFromFlatList [trace ("element " ++ show a ++ " " ++ show b) element a b p, element (a+1) b p, element a (b+1) p, element (a+1) (b+1) p]
      buildThreePack a b p = patternFromFlatList [element (a+i) (b+j) p | i<-[0..2], j<-[0..2]]
      patternSize = size pattern
      halfPatternSize = div patternSize 2
      thirdPatternSize = div patternSize 3
      sizeMultipleOf2 = even patternSize

fitBackTogether patterns = where
   newPatternSize = smallPatternSizes * ((round . sqrt . fromIntegral . length) patterns)
   smallPatternSizes = (size $ head patterns) -- 2 or 3
   smallPatternsPerLine = div newPatternSize smallPatternSizes

main = do
   rawInput <- getContents
   let rules = map ruleFromLine (lines rawInput)
   let a = matchAny startPattern rules
   print a
   print $ breakInPacks a
