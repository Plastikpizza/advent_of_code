data Spinlock = Spinlock {pos::Int, circle::[Int]} deriving (Show)

insertAfter v 0 (l:r) = l : v : r
insertAfter v i (l:r) = l : insertAfter v (i-1) r

newSpinlock = Spinlock 0 [0]

insertSpinlock v s = Spinlock (nextPos+1) nextCircle
   where
      nextPos = (mod ((pos s)+348) (length $ circle s))
      nextCircle = insertAfter v nextPos (circle s)

spin 1 = insertSpinlock 1 newSpinlock
spin n = insertSpinlock n $ spin (n-1)

partOne n = circle s !! (pos s +1)
   where
      s = spin n

partTwo 0 = return 0
partTwo n = do
   las <- partTwo (n-1)
   let val = (mod (las +348) n) + 1
   if val == 1 then
      do
         putStrLn ("insert to 1 at " ++ show n)
         return val
   else
      do
         return val

main = do
   -- part one
   print $ partOne 2017
   -- part two
   partTwo 50000000
