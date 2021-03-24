--Vadim Karpinskiy
--ex1.2
add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z

------------------------------------------------------

--ex1.3
add3C :: Num a => (a, a) -> a -> a
add3C (x,y) z = x + y + z

-- ghci> add3C (1,2) 3

------------------------------------------------------

--ex 2.1
fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_toPower5 :: Num a => a -> a
_toPower5 = (^5)

substrNFrom5 :: Num a => a -> a
substrNFrom5 = (5 -)

substr5From_ :: Num a => a -> a
substr5From_ = (+ (-5))

-------------------------------------------------------

--ex 2.2

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x

-------------------------------------------------------

--ex 4.1
isPalindrome :: [Char] -> Bool
isPalindrome s  | length s <= 1 = True
		        | head s /= last s = False
		        | otherwise = isPalindrome (tail (init s))

-------------------------------------------------------

--ex 5.1
[(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2+b^2==c^2]

-------------------------------------------------------

--ex 6.7.1

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x:xs) = x + prod' xs

length' :: [a] -> Int 
length' [] = 0
length (_:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = x && and' xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = x*x : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = 
        if x `mod` 2 == 0 then x : selectEven xs
        else selectEven xs

-----------------------------------------------------

--ex7.1

prod'2 :: Num a => [a] -> a
prod'2 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

length'2 :: Num a => [a] -> a
length'2 = loop 0
 where loop acc []     = acc
       loop acc (_:xs) = loop (1 + acc) xs

----------------------------------------------------

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<= x) xs
   rightPart xs = filter (> x) xs