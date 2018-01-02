import Data.Array
-- import Debug.Trace(trace)
trace a b = b

data CPU = CPU {programCounter::Int,
                program::Array Int [String],
                registers::Array Char Int}
   deriving (Eq)

instance Show CPU where
   show cpu = show $ registers cpu

newCpu = let reg = (array ('a', 'h') $ zip ['a'..'h'] $ replicate 8 0) in
      CPU 0 (array (0,0) []) reg

jump cpu n = cpu{programCounter=((programCounter cpu) +n)}

toValue p cpu
   | elem (head p) "-0987654321" = read p
   | otherwise = (registers cpu)!(head p)

excecute cpu (opc:arg1:arg2:[])
   | opc == "set" = jump cpu{registers=newlySet} 1
   | opc == "sub" = jump cpu{registers=afterSub} 1
   | opc == "mul" = jump cpu{registers=afterMul} 1
   | opc == "jnz" = jump cpu jumpingD
   | otherwise = cpu
   where
      i1 = head arg1
      i2 = toValue arg2 cpu
      v1 = (registers cpu) ! i1
      newlySet = (registers cpu)//[(i1, i2)]
      afterSub = (registers cpu)//[(i1, v1-i2)]
      afterMul = (registers cpu)//[(i1, v1*i2)]
      jumpingD = if toValue arg1 cpu /= 0 then i2 else 1

currentCommand cpu = program cpu ! programCounter cpu
currentInstruction = head . currentCommand

countMul cpu
   | illegalPC = 0
   | curInstrc == "mul" = (countMul $ excecute cpu (currentCommand cpu)) +1
   | otherwise = countMul $ excecute cpu (currentCommand cpu)
   where
      illegalPC = (programCounter cpu < 0) || (programCounter cpu >= endOfProg)
      endOfProg = (length $ program cpu)
      curInstrc = currentInstruction cpu

isPrime n = check n 2 where
   check j l
      | mod j l == 0 = False
      | l*l > j = True
      | otherwise = check j (l+1)

main = do
   rawInput <- getContents
   let instructionList = map words (lines rawInput)
   let zipped = zip [0..] instructionList
   let instructions = array (0,(length instructionList) -1) zipped
   print $ countMul newCpu{program=instructions}
   print $ length [i | i <- [105700, 105717..122701], not $ isPrime i]
