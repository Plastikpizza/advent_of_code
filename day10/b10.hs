import A10 (State, step, startState, list)
import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)

sparseHash s []     = s
sparseHash s (l:ls) = sparseHash (step s l) ls

xorList (l:r) = foldl xor l r

denseHash [] = []
denseHash  s = (xorList (take 16 s)) : (denseHash (drop 16 s))

main = do
   rawLengths <- getContents
   let lengths = concat $ take 64 $ repeat ((map ord (head $ lines rawLengths)) ++ [17,31,73,47,23])
   let sh = sparseHash startState lengths
   let dh = denseHash (list sh)
   let so = concat $ map (\a -> showHex a "") dh
   putStrLn so
