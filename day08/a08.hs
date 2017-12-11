import qualified Data.Map as Map

getRegister :: String -> Map.Map String Int -> Int
getRegister register cpu = if Map.lookup register cpu == Nothing then 0 else (Map.!) cpu register

setRegister :: String -> Int -> Map.Map String Int -> Map.Map String Int
setRegister = Map.insert

interpreteInstruction (register:operation:operatorStr:_:ifregist:boolchck:chckagaiStr:line) cpu
   | boolchck == "<" && cmpValReg < chckagai  = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | boolchck == ">" && cmpValReg > chckagai = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | boolchck == "!=" && chckagai /= cmpValReg = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | boolchck == "<=" && cmpValReg <= chckagai = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | boolchck == ">=" && cmpValReg >= chckagai = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | boolchck == "==" && chckagai == cmpValReg = case operation of
                           "inc" -> setRegister register ((getRegister register cpu) + operator) cpu
                           "dec" -> setRegister register ((getRegister register cpu) - operator) cpu
   | otherwise = cpu
   where
      operator::Int
      operator  = read operatorStr
      chckagai::Int
      chckagai  = read chckagaiStr
      cmpValReg = getRegister ifregist cpu

runInstructions [] cpu = cpu
runInstructions (l:r) cpu = runInstructions r (interpreteInstruction (words l) cpu)

runInstructionsWithLookingForMax :: [String] -> Map.Map String Int -> Int -> Int
runInstructionsWithLookingForMax [] cpu m = maximum [m,maximum (0 : Map.elems cpu)]
runInstructionsWithLookingForMax (l:r) cpu m
   | v > m = runInstructionsWithLookingForMax (l:r) cpu v
   | otherwise = runInstructionsWithLookingForMax r (interpreteInstruction (words l) cpu) m
   where
      v = maximum (0 : Map.elems cpu)

main = do
   input <- getContents
   let instructions = lines input
   let fin = runInstructions instructions (Map.fromList [])
   print $ maximum (Map.elems fin)
   print $ runInstructionsWithLookingForMax instructions (Map.fromList []) 0
