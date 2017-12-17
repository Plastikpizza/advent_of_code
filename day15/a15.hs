import Data.Bits(xor, (.&.))

data Generator = Generator {current::Int, factor::Int, modulo::Int}
   deriving (Eq, Show)

generatorA = Generator 516 16807 2147483647
generatorB = Generator 190 48271 2147483647

generate g = g{current=mod (current g * factor g) (modulo g)}

generateDividable g n = if mod (current (generate g)) n /= 0
                        then generateDividable (generate g) n
                        else generate g

match a b = (b .&. 65535) == (a .&. 65535)

partOne 0 a b = if match (current a) (current b) then 1 else 0
partOne n a b = if match (current a) (current b) then 1 + s else s
   where
      s = partOne (n-1) (generate a) (generate b)


partTwo 0 a b = if match (current a) (current b) then 1 else 0
partTwo n a b = if match (current a) (current b) then 1 + s else s
   where
      s = partTwo (n-1) (generateDividable a 4) (generateDividable b 8)

main = do
   print $ partOne 40000000 generatorA generatorB
   print $ partTwo  5000000 generatorA generatorB
