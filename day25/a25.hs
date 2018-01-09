import qualified Data.Map as Map
data Bin = O | I deriving (Eq, Show, Ord)
data TuringMachine = TuringMachine {tape::Map.Map Int Bin,
                                    cursor::Int,
                                    state::State}
data State = State {check::TuringMachine->Bool,
                    negative::TuringMachine->TuringMachine,
                    positive::TuringMachine->TuringMachine}

readAtCursor tm = if Map.lookup (cursor tm) (tape tm) == Nothing then False
   else True

writeAtCursor v tm = tm{tape=if v == I then newTape else delTape} where
   newTape = Map.insert (cursor tm) I delTape
   delTape = Map.delete (cursor tm) (tape tm)
deltaCursor d tm = tm{cursor=cursor tm + d}
setState s tm = tm{state=s}
stdCycle v d s = (setState s . deltaCursor d . writeAtCursor v)
newTm = TuringMachine (Map.empty) 0 stateA
stdState = State readAtCursor
right =    1
left  =  (-1)

stateA = stdState (stdCycle I right stateB) (stdCycle O left  stateD)
stateB = stdState (stdCycle I right stateC) (stdCycle O right stateF)
stateC = stdState (stdCycle I left  stateC) (stdCycle I left  stateA)
stateD = stdState (stdCycle O left  stateE) (stdCycle I right stateA)
stateE = stdState (stdCycle I left  stateA) (stdCycle O right stateB)
stateF = stdState (stdCycle O right stateC) (stdCycle O right stateE)

step tm
   | check (state tm) tm = positive (state tm) tm
   | otherwise = negative (state tm) tm

steps 0 tm = tm
steps n tm = steps (n-1) (step tm)

partOne = length $ tape $ steps 12317297 newTm

main = do
   print partOne
