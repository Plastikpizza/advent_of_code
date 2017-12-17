data Scanner = Scanner {position::Int, height::Int, layer::Int, rising::Bool} deriving (Show, Eq)

pos t s = if odd $ div t (s-1) then (s-1) - mod t (s-1) else mod t (s-1)

parseFirewall :: String -> [Scanner]
parseFirewall string = map (newScanner . readPair) (lines string)
   where
      readPair :: String -> [Int]
      readPair s = map read $ words [c | c <- s, elem c "0987654321 "]
      newScanner l = Scanner 0 (last l) (head l) False

main = do
   rawInput <- getContents
   let firewall = parseFirewall rawInput
   let f o d = [(layer s+o, height s, pos (layer s +d) (height s)) | s <- firewall]
   let s o d = sum [a*b | (a,b,c) <- f o d, c == 0]
   print $ s 0 0
   print $ (length $ takeWhile (/=0) (map (s 1) [1..])) +1
