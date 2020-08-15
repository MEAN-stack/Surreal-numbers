import Data.List

data Surreal = Surreal ([Surreal], [Surreal]) deriving Show

zero = Surreal ([], [])

one = Surreal ([zero], [])
two = Surreal ([one], [])
three = Surreal ([two], [])

minus = Surreal ([], [zero])
minus_two = Surreal ([], [minus])
minus_three = Surreal ([], [minus_two])

half = Surreal ([zero], [one])
quarter = Surreal ([zero], [half])
three_quarter = Surreal ([half], [one])


a = Surreal ([],[])
b = Surreal ([minus], [])
c = Surreal ([zero], [])
d = Surreal ([one], [])
e = Surreal ([minus, zero], [])
f = Surreal ([minus, one], [])
g = Surreal ([zero, one], [])
h = Surreal ([minus, zero, one], [])
i = Surreal ([], [minus])
j = Surreal ([], [zero])
k = Surreal ([], [one])
l = Surreal ([], [minus, zero])
m = Surreal ([], [minus, one])
n = Surreal ([], [zero, one])
o = Surreal ([], [minus, zero, one])
p = Surreal ([minus], [zero])
q = Surreal ([zero], [one])
r = Surreal ([minus], [one])
s = Surreal ([minus, zero], [one])
t = Surreal ([minus], [zero, one])

-- i < minus < p < zero < q < one < d
-- s == q
-- l == i
-- b == k

-- X = (xl, xr)
-- Y = (yl, yr)
--
-- X <= Y if and only if no x in xl >= Y and no y in yr <= X

eq :: Surreal -> Surreal -> Bool
eq x y = x `le` y && y `le` x

ge :: Surreal -> Surreal -> Bool
ge = flip le

le :: Surreal -> Surreal -> Bool
le (Surreal (xls, xrs)) (Surreal (yls, yrs)) = if (length [y | y <- yrs, y `le` (Surreal (xls, xrs))] == 0) && (length [x | x <- xls, x `ge` (Surreal (yls, yrs))] == 0) then True else False

instance Eq Surreal where (==) = eq

instance Ord Surreal where
    compare x y = if x==y then EQ
                  else if x `le` y
                       then LT
                       else GT

reduce :: Surreal -> Surreal
reduce (Surreal ([], [])) = Surreal ([], [])
reduce (Surreal (ls, [])) = Surreal ([(last.sort) ls], []) 
reduce (Surreal ([], rs)) = Surreal ([], [(head.sort) rs])
reduce (Surreal (ls, rs)) = Surreal ([(reduce.last.sort) ls], [(reduce.head.sort) rs])

leftSet :: Surreal -> [Surreal]
leftSet (Surreal (l, r)) = l

rightSet :: Surreal -> [Surreal]
rightSet (Surreal (l, r)) = r

toInt :: Surreal -> Integer
toInt (Surreal ([], [])) = 0
toInt (Surreal ([n], [])) = toInt n + 1
toInt (Surreal ([], [n])) = toInt n - 1
toInt x = 42

fromInt :: Integer -> Surreal
fromInt 0 = Surreal ([], [])
fromInt n = if n > 0
            then Surreal ([fromInt (n-1)], [])
            else Surreal ([], [fromInt (n+1)])

add :: Surreal -> Surreal -> Surreal
add x@(Surreal (xls, xrs)) y@(Surreal (yls, yrs)) = Surreal (zls, zrs)
                                                    where zls = [add xl y | xl <- xls] `union` [add x yl | yl <- yls]
                                                          zrs = [add xr y | xr <- xrs] `union` [add x yr | yr <- yrs]

sub :: Surreal -> Surreal -> Surreal
sub x y = add x (neg y)

neg :: Surreal -> Surreal
neg (Surreal (xls, xrs)) = Surreal (yls, yrs)
                           where yls = [neg x | x <- xrs]
                                 yrs = [neg x | x <- xls]

mul :: Surreal -> Surreal -> Surreal
mul x@(Surreal (xls, xrs)) y@(Surreal (yls, yrs)) = Surreal (zls, zrs)
    where zls = [((xl `mul` y) `add` (x `mul` yl)) `sub` (xl `mul` yl) | xl<-xls, yl<-yls] `union` [((xr `mul` y) `add` (x `mul` yr)) `sub` (xr `mul` yr) | xr<-xrs, yr<-yrs]
          zrs = [((xl `mul` y) `add` (x `mul` yr)) `sub` (xl `mul` yr) | xl<-xls, yr<-yrs] `union` [((x `mul` yl) `add` (xr `mul` y)) `sub` (xr `mul` yl) | xr<-xrs, yl<-yls]

main = do
    putStrLn $ show zero
    putStrLn $ show $ fromInt 1
    putStrLn $ show $ fromInt 2
    putStrLn $ show $ fromInt 3
    putStrLn $ show $ fromInt 4
    putStrLn $ show $ fromInt 5
    putStrLn $ show $ fromInt 6
    putStrLn $ show $ three `mul` two
