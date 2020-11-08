----module Main where
import Prelude hiding (length)
--import Lib

--main :: IO ()
--main = someFunc
lst_ :: [Integer]
lst_ = map f lst
f :: Num a => a -> a
f x = x*(x+1)

flt_ :: [Integer]
flt_ = filter e xs
xs :: [Integer]
xs = [1..10]
e :: Integral a => a -> Bool
e x = x `mod` 2 == 0

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

{-|
For lists, the base case is the empty list [], the induction case is adding an element to the list x:xs
-}

-- speller :: [[Char]] -> [Char]
-- forM_ :: [a] -> (a -> IO b) -> IO ()
-- https://gist.github.com/delewit/a8c665967a9cf9d784add022628071f8

--Even though submissions are over, here's my version:
--speller :: [[Char]] -> [Char]
--speller lst = foldl join "" (map spell lst)

spell :: [Char] -> [Char]
spell "" = ""
spell word =
    let (firstLetter:_) = word
        in [firstLetter] ++ " is for " ++ word

join :: [Char] -> [Char] -> [Char]
join "" b = b
join a "" = a
join a b = a ++ ", " ++ b

-- data types

-- ::Int
-- ::Char
-- ::Double - what is double?
-- ::String - what is the difference between char and string
-- ::Bool

-- data defines new new type 
-- ex. data SimpleNum = One | Two | Many deriving Show
-- :t One



--aa :: Double
aa :: Double
aa = foldl (/) 16 [8,4,2,1]
-- answer 
-- 16 / 8 == 2
-- 16 / 4 == 4
-- 16 / 2 == 8
-- 16 / 1 == 16
-- 16 / 8 / 4 / 2
{-|
foldl works from left to right and divides the accumulator by the list element.
16/8 = 2.0 2.0 / 4 = 0.5 0.5 / 2 = 0.25. 0.25 / 1 = 0.25.
-}


-- come back to check
ab :: Double
ab = foldr (/) 16 [4,8]
-- answer 
-- 8 / 16 = 0.5
-- 4 / 16 = 0.25
-- 2 / 16 = 0.125
-- 1 / 16 = 6.25e-2 - er what?


-- !IMPORTANT map foldl foldr predicate
-- map :: (a -> b) -> [a] -> [b]
-- foldl :: (b -> a -> b) -> b -> [a] -> b 
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- filter :: (a -> Bool) -> [a] -> [a]:

{-|
filter pred lst
  | null lst = []
  | otherwise = if pred x 
     then x:filter pred xs
     else filter pred xs
       where x:xs=lst
    -}

--lst :: [Integer]
lst :: [Integer]
lst = [2,4,6]

a :: Integer
a = 1

--b :: (a -> Bool) -> [a] -> [a]
--b = filter :: (a->Bool) -> [a] -> [a]
c :: [Integer]
c = filter (<5) [3,9,2,12,6,4]

-- working foldr
veryBigList :: [Integer]
veryBigList = [1..1000000]
foldr' :: (t -> a -> t) -> t -> [a] -> t
foldr' f z []     = z
foldr' f z (x:xs) = let z' = z `f` x 
                    in seq z' $ foldr' f z' xs
sum3 :: [Integer] -> Integer
sum3 = foldr' (+) 0

main :: IO ()
main = do
    print lst_
    print a
    print c
    print x
    print flt_

list :: [Double]
list = [10,50,100]
g :: Double -> Double -> Double
g = (/)
accl :: Double
accl = foldr g 10 list
length :: Eq a => [a] -> Int
length lst =
  if lst == []
    then 0
    else let x:xs = lst in 1 + length xs

fun :: Integer -> Integer
fun = fun' where fun' 1 = 0; fun' x = x + fun' (x-1)
x = filter (<5) [3,9,2,12,6,4]

-- speller :: [[Char]] -> [Char]
-- speller ["abacus"];



-- g division, accumulater then call function
-- answer
 
    -- print accl
{-|
        As we have already seen, a list is built from the empty list [] and the function cons or in operator form (:).
        Every list must be either
        [] or
        (x:xs) for some x (the head of the list) and xs (the tail).
        where (x:xs) is an alternative syntax for cons x xs

        The recursive definition follows the structure of the data:
        Base case of the recursion is [].
        Recursion (or induction) case is (x:xs).

        However, in order to create such recursive definitions, 
        we must first see how we can create conditional functions: functions that define both the base case and the induction case.
-}