import Data.Array

data CPU = CPU{registers::Array Char Int, program::Array Int [String],
   messages::[Int], pc::Int, sendCount::Int,name::String, other::CPU}
   | NotInitialized deriving (Eq)

newCpu p = CPU registerArray p [] 0 0 ""
   where
      registerArray = array ('a', 'z') $ zip ['a'..'z'] $ take 26 $ repeat 0

toValue x cpu
   | elem (head x) "1234567890- " = read x
   | otherwise = (registers cpu) ! (head x)

set r v cpu = cpu{registers=registers cpu//[updatePair],pc=(pc cpu)+1}
   where
      updatePair = (head r, toValue v cpu)

currentCommand c = head $ program c ! pc c

stuck cpu = (messages cpu)==[] && (currentCommand cpu)=="rcv"

execute cpu = case cmd of
   "set" -> set arg1 arg2 cpu
   "mul" -> set arg1 (show ((toValue arg1 cpu)*(toValue arg2 cpu))) cpu
   "mod" -> set arg1 (show (mod (toValue arg1 cpu) (toValue arg2 cpu))) cpu
   "add" -> set arg1 (show ((toValue arg1 cpu)+(toValue arg2 cpu))) cpu
   "snd" -> cpu{messages=(messages cpu)++[toValue arg1 cpu],
                pc=(pc cpu)+1, sendCount=(sendCount )cpu +1}
   "rcv" -> case (messages . other) cpu of
      [] -> (other cpu){other=cpu}
      otherwise -> (set arg1 nextMessage cpu){other=newOther}
   "jgz" -> if toValue arg1 cpu > 0
      then
         cpu{pc=((pc cpu) + (toValue arg2 cpu))}
      else
         cpu{pc=(pc cpu)+1}
   where
      cmd  = currentInstruction!!0
      arg1 = currentInstruction!!1
      arg2 = currentInstruction!!2
      currentInstruction = program cpu ! pc cpu
      newOther = (other cpu){messages=(tail . messages . other) cpu}
      nextMessage = (show ((head . messages . other) cpu))

partTwo cpu
   | stuck cpu && stuck (other cpu) =
      (name cpu, sendCount cpu, name $ other cpu, sendCount $ other cpu)
   | otherwise = (partTwo . execute) cpu

main = do
   rawInput <- getContents
   let duetList = map words (lines rawInput)
   let duet = array (0, length duetList -1) $ zip [0,1..] duetList
   let cpu0 = (newCpu duet NotInitialized){name="cpu0"}
   let cpu1 = set "p" "1" (newCpu duet cpu0{other=cpu1}){name="cpu1"}
   print $ partTwo cpu1
