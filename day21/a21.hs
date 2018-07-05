import Debug.Trace (trace)
data Square =
   One Char
   | Two {a::Square,b::Square,
          c::Square,d::Square}
   | Three {a::Square,b::Square,c::Square,
            d::Square,e::Square,f::Square,
            g::Square,h::Square,i::Square}
   deriving (Eq, Show)

data Rule = Rule {input::Square, output::Square} deriving (Show)


turn (One a) = One a
turn (Two a b c d)
   = Two (turn b) (turn d)
         (turn a) (turn c)
turn (Three a b c d e f g h i)
   = Three (turn c) (turn f) (turn i)
           (turn b) (turn e) (turn h)
           (turn a) (turn d) (turn g)
mirror (One a) = One a
mirror (Two a b c d)
   = Two (mirror b) (mirror a)
         (mirror d) (mirror c)
mirror (Three a b c d e f g h i)
   = Three (mirror c) (mirror b) (mirror a)
           (mirror f) (mirror e) (mirror d)
           (mirror i) (mirror h) (mirror g)

parseSquare (a:b:'/':c:d:[])
   = Two (One a) (One b)
         (One c) (One d)
parseSquare (a:b:c:'/':d:e:f:'/':g:h:i:[])
   = Three (One a) (One b) (One c)
           (One d) (One e) (One f)
           (One g) (One h) (One i)
parseSquare (a:b:c:d:'/':e:f:g:h:'/':i:j:k:l:'/':m:n:o:p:[])
   = Two (Two (One a) (One b)
              (One e) (One f)) (Two (One c) (One d)
                                    (One g) (One h))
         (Two (One i) (One j)
              (One m) (One n)) (Two (One k) (One l)
                                    (One o) (One p))

parseRule string = Rule (parseSquare first) (parseSquare backmost)
   where tokens   = words string
         first    = head tokens
         backmost = last tokens

allMutations square = [a,b,c,d] ++ map mirror [a,b,c,d] where
   a = turn square
   b = turn a
   c = turn b
   d = square

match s r = elem s (allMutations $ input r)

swap  (Two (Three aa ab ba
                  ac ad bc
                  da db ea) (Three bb ca cb
                                   bd cc cd
                                   eb fa fb)
           (Three dc dd ec
                  ga gb ha
                  gc gd hc) (Three ed fc fd
                                   hb ia ib
                                   hd ic idd))
      = (Three (Two aa ab
                    ac ad) (Two ba bb
                                bc bd) (Two ca cb
                                            cc cd)
               (Two da db
                    dc dd) (Two ea eb
                                ec ed) (Two fa fb
                                            fc fd)
               (Two ga gb
                    gc gd) (Two ha hb
                                hc hd) (Two ia ib
                                            ic idd))

swap (Two a b c d) = Two (swap a) (swap b) (swap c) (swap d)
swap (Three a b c d e f g h i) = Three (swap a) (swap b) (swap c) (swap d) (swap e) (swap f) (swap g) (swap h) (swap i)
swap a = a

replace s@(Two a b c d) rs
   | length matches == 0 = (Two (replace a rs) (replace b rs) (replace c rs) (replace d rs))
   | length matches == 1 = output $ head matches
   where matches = filter (match s) rs
replace s@(Three a b c d e f g h i) rs
   | length matches == 0 = (Three (replace a rs) (replace b rs) (replace c rs) (replace d rs) (replace e rs) (replace f rs) (replace g rs) (replace h rs) (replace i rs))
   | length matches == 1 = output $ head matches
   where matches = filter (match s) rs

times 0 f a = a
times n f a = times (n-1) f (f a)

count e (Three a b c d p f g h i) = count e a + count e b + count e c + count e d + count e p + count e f + count e g + count e h + count e i
count e (Two a b c d) = count e a + count e b + count e c + count e d
count e (One a) = if a == e then 1 else 0

start = parseSquare ".#./..#/###"
main = do
   rules_in <- fmap lines getContents
   let rules = map parseRule rules_in
   print $ count '#' $ times 18 (flip (replace . swap) rules) start
