processString "" o s g c = (s, c)
processString ('!':r) o s g c = processString (tail r) o s g c
processString ('<':r) o s False c = processString r o s True c
processString ('>':r) o s g c = processString r o s False c
processString ('{':r) o s False c = processString r (o+1) s False c
processString ('}':r) o s False c = processString r (o-1) (s+o) False c
processString (_:r) o s True c = processString r o s True (c+1)
processString (_:r) o s False c = processString r o s False c

dayNine string = processString string 0 0 False 0

main = do
   input <- getContents
   -- part one and two
   print $ dayNine input
