import Data.Array
import Split

initialSetting = array (0,15) $ zip [0..(15::Int)] ['a'..'p']

spin 0 s = s
spin n s = spin (n-1) (s//[(mod (i+1) 16, s!i) | i <- [0..15]])

exch n m s = s//[(n, s!m),(m, s!n)]

indexOf n s = head [i | i <- [0..15], s!i == n]

interprete ins setting
   | head ins == 's' = spin sn setting
   | head ins == 'x' = exch xn xm setting
   | head ins == 'p' = exch pn pm setting
   where
      sn = read $ tail ins
      param = splitOn "/" $ tail ins
      xn = (read $ head param)::Int
      xm = (read $ last param)::Int
      pn = indexOf (head $ head param) setting
      pm = indexOf (head $ last param) setting

interpreteAll instructions setting = foldl (flip interprete) setting instructions

apply 0 f i = i
apply n f i = apply (n-1) f (f i)

circle s1 s2 ins
   | s1 == s2 = 1
   | otherwise = 1 + circle s1 (interpreteAll ins s2) ins

main = do
   rawInput <- getContents
   let input = splitOn "," rawInput
   print $ elems $ apply 1 (interpreteAll input) initialSetting
   let circleLength = circle initialSetting (interpreteAll input initialSetting) input
   let rest = mod 1000000000 circleLength
   print $ elems $ apply rest (interpreteAll input) initialSetting
