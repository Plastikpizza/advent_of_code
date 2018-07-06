module A10 where
data State = State {list::[Int], pos::Int, ssz::Int} deriving (Show, Eq)

rotate     n l = (drop m l)++(take m l) where m = mod n (length l)
rotateBack n l = (drop m l)++(take m l) where m = (length l) - mod n (length l)
reverseNum n l = (reverse $ take n l) ++ drop n l

startState = State [0..255] 0 0
step s l = s{list=rotateBack (pos s) $ reverseNum l $ rotate (pos s) $ list s, pos= mod ((pos s) + l + (ssz s)) (length $ list s), ssz=(ssz s)+1}

t1 s []     = product $ take 2 $ list s
t1 s (l:ls) = t1 (step s l) ls

main = do
   rawLengths <- getContents
   let lengths = map (read) (words [(\a -> if a == ',' then ' ' else a) c | c <- rawLengths]) :: [Int]
   print $ t1 startState lengths
