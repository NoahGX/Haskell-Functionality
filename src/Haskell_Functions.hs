------------- CS 420 : Advanced Programming Languages -----------------

---------- Assignment 2 : Functional Prgamming on Haskell -------------

-----------------------------------------------------------------------


-- Runs the script without including prelude library 
{-# LANGUAGE NoImplicitPrelude #-}


module Assign2 where

-- import basic functions needed from prelude library
-- all element functions are supported.
-- map and elem are supported. 

import ModifiedPrelude

-----------------------------------------------------------------------

-- PART A

-- Question 1

-- Sum of all the elements of a list
--
-- >>> sumList [1, 2, 3, 4]
-- 10
--
-- >>> sumList [1, -2, 3, 5]
-- 7
--
-- >>> sumList [1, 3, 5, 7, 9, 11]
-- 36

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs


-- Question 2

-- `digitsOfInt n` should return `[]` if `n` is <=0,
--    and otherwise returns the list of digits of `n` in the
--    order in which they appear
--
-- >>> digitsOfInt 3124
-- [3, 1, 2, 4]
--
-- >>> digitsOfInt 352663
-- [3, 5, 2, 6, 6, 3]

digitsOfInt :: Int -> [Int]
digitsOfInt n | n <= 0 = []
        | otherwise = digitsOfInt (n `div` 10) ++ [n `mod` 10]


-- Question 3

-- | `digits n` retruns the list of digits of `n`
--
-- >>> digits 31243
-- [3,1,2,4,3]
--
-- digits (-23422)
-- [2, 3, 4, 2, 2]

digits :: Int -> [Int]
digits 0 = []
digits n | n < 0 = digits (n*(-1))
        | otherwise = digits (n `div` 10) ++ [n `mod` 10]


-- Question 4

--   Additive persistence is the process of taking a number, adding its digits,
--   then adding the digits of the number derived from it, etc.,
--   until the remaining number has only one digit.
--
--   The number of additions required to obtain a single digit
--   from a number n is called the additive persistence of n.
--
--   For example, the sequence obtained from the starting number
--   9876 is (9876, 30, 3), so 9876 has
--   an additive persistence of 2
--
-- NOTE: assume additivePersistence is only called with positive numbers
--
-- >>> additivePersistence 9876
-- 2

myNumSum :: Int -> Int
myNumSum n | n == 0 = 0
        | otherwise = (n `mod` 10) + myNumSum (n `div` 10)

additivePersistence :: Int -> Int
additivePersistence n | n < 10 = 0
        | otherwise = additivePersistence (myNumSum n) + 1


-- Question 5

--   DigitalRoot n is the digit obtained at the end of the sequence
--   computing the additivePersistence
--
--   For example, the sequence obtained from the starting number
--   9876 is (9876, 30, 3), so 9876 has
--   a digital root of 3.
--
-- NOTE: assume digitalRoot is only called with positive numbers
--
-- >>> digitalRoot 9876
-- 3

digitalRoot :: Int -> Int
digitalRoot n | n < 10 = n
        | otherwise = digitalRoot (myNumSum n)


-- Question 6

-- listReverse [x1,x2,...,xn] returns [xn,...,x2,x1]
--
-- >>> listReverse []
-- []
--
-- >>> listReverse [1,2,3,4]
-- [4,3,2,1]
--
-- >>> listReverse ["i", "want", "to", "ride", "my", "bicycle"]
-- ["bicycle", "my", "ride", "to", "want", "i"]

listReverse :: [a] -> [a]
listReverse xs = reversedList xs []
    where
        reversedList [] newList = newList
        reversedList (x:xss) newList = reversedList xss (x:newList)


-- Question 7

-- A string which has the property of reading the same 
-- forwards as it does backwards
--
-- >>> palindrome "malayalam"
-- True
--
-- >>> palindrome "myxomatosis"
-- False

palindrome :: String -> Bool
palindrome x = x == listReverse x


-- Question 8
--
-- Compute all the digital rool of all elements in the list 
--
-- >>> rootList [10, 11, 12]
-- [1, 2, 3]

rootList :: [Int] -> [Int]
rootList [] = []
rootList xs = [myNumSum x | x <- xs]


-----------------------------------------------------------------------

-- PART B

-- Questions 9 to 13 should be solved using recurrsion ONLY


-- Question 9

--  `assoc def key [(k1,v1), (k2,v2), (k3,v3);...])`
--   searches the list for the first i such that `ki` = `key`.
--   If such a ki is found, then vi is returned.
--   Otherwise, if no such ki exists in the list, `def` is returned.
--

-- >>> assoc 0 "william" [("ranjit", 85), ("william",23), ("moose",44)])
-- 23
--
-- >>> assoc 0 "bob" [("ranjit",85), ("william",23), ("moose",44)]
-- 0

assoc :: Int -> String -> [(String, Int)] -> Int
assoc def _ [] = 0
assoc def key [] = 0
assoc def key ((x,y):xs) = if x == key
        then y
        else assoc def key xs


-- Question 10

--   returns the list of elements of `l` with duplicates
--   that is, second, third ... occurrences, removed,
--   and where the remaining elements appear in the
--   same order as in l.

-- >>> removeDuplicates [1,6,2,4,12,2,13,12,6,9,13]
-- [1,6,2,4,12,13,9]

removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = duplicatesList xs []
    where
        duplicatesList [] newList = listReverse newList
        duplicatesList (x:xss) newList | x `elem` newList = duplicatesList xss newList
            | otherwise = duplicatesList xss (x:newList)


-- Question 11

-- `wwhile f x` returns `x'` where there exist values
--      `v_0`,...,`v_n` such that
--      - `x` is equal to `v_0`
--      - `x'` is equal to `v_n`
--      - for each `i` between `0` and `n-2`, we have `f v_i` equals `(true, v_i+1)`
--      - `f v_n-1` equals `(false, v_n)`.

ff:: Int -> (Bool, Int)
ff x = (xx < 100, xx)
      where xx = x * x * x

-- >>> wwhile ff 2
-- 512

wwhile :: (a -> (Bool, a)) -> a -> a
wwhile whileFunc x = let (bool, xx) = whileFunc x
        in if bool then wwhile whileFunc xx
        else xx


-- Question 12

-- `fixpointL f x0` which returns
--     the list [x_0, x_1, x_2, x_3, ... , x_n]
--     where
--     * x = x_0
--     * f x_0 = x_1, f x_1 = x_2, f x_2 = x_3, ... f x_n = x_{n+1}
--     * xn = x_{n+1}

collatz :: Int -> Int
collatz 1 = 1
collatz n | even n    = n `div` 2
          | otherwise = 3 * n + 1

gg :: Int -> Int
gg x = truncate (1e6 * cos (1e-6 * fromIntegral x))

-- >>> fixpointL collatz 1
-- [1]
-- >>> fixpointL collatz 2
-- [2,1]
-- >>> fixpointL collatz 3
-- [3,5,8,2,1]
-- >>> fixpointL collatz 4
-- [4,1]
-- >>> fixpointL collatz 5
-- [5,8,2,1]
-- >>> fixpointL gg 0
-- [0, 1000000, 540302, 857553, 654289, 793480,701369,763959,
--  722102,750418,731403,744238,735604,741425,737506,740147,738369,
--  739567,738760,739304,738937,739184,739018,739130,739054,739106,
--  739071,739094,739079,739089,739082,739087,739083,739086,739084,739085]
-- this is because cos 0.739085 is approximately 0.739085

fixpointL :: (Int -> Int) -> Int -> [Int]
fixpointL colFunc x = newList x
    where
        newList x1 |  x1 == colFunc x1 = [x1]
            | otherwise = x1:newList (colFunc x1)


-- Question 12

-- Now refactor your implementation of `fixpointL` so that it just returns
-- the LAST element of the list, i.e. the `xn` that is equal to `f xn`

-- >>> fixpointW collatz 1
-- 1
-- >>> fixpointW collatz 2
-- 1
-- >>> fixpointW collatz 3
-- 1
-- >>> fixpointW collatz 4
-- 1
-- >>> fixpointW collatz 5
-- 1
-- >>> fixpointW g 0
-- 739085

fixpointW :: (Int -> Int) -> Int -> Int 
fixpointW colFunc x | x == colFunc x = x
            | otherwise = fixpointW colFunc (colFunc x)


-----------------------------------------------------------------------

-- PART C

-- Questions 14 to 16 should be solved using foldr ONLY


-- Question 14

-- sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum = foldr (\x xs -> x*x + xs) 0


-- Question 15

-- `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24

pipe :: [(a -> a)] -> (a -> a)
pipe fs x = foldr (\func1 func2 -> func1 func2) x fs


-- Question 16

-- `sepConcat sep [s1,...,sn]` returns 
--         `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep [] = ""
sepConcat sep [s] = s
sepConcat sep (x:xs) = foldl' (\string1 string2 -> string1 ++ sep ++ string2) x xs


-- Question 17

-- This question does not require recurrsion or fold
-- Solve it using in-build map function

intString :: Int -> String
intString = show

-- `stringOfList pp [x1,...,xn]` uses the element-wise 
--  printer `pp` to convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
stringOfList showFunc xs = "[" ++ sepConcat ", " (map showFunc xs) ++ "]"


-- Question 18 & 19 uses a new type
-- BigInt = [Int]

type BigInt = [Int]


-- Question 18 
-- You will be writing three helper functions to solve Question 19 

-- `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
clone x n | n < 0 = []
        | n == 0 = []
        | otherwise = x:clone x (n-1)


-- `padZero l1 l2` returns a pair (l1', l2') which are the input lists,
--  padded with extra `0` on the left such that the lengths of `l1'` 
--  and `l2'` are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- ([0,0,9,9], [1,0,0,2])
--
-- >>> padZero [1,0,0,2] [9,9]
-- ([1,0,0,2], [0,0,9,9])

padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero xs xss = listComp (length xs - length xss)
    where
        listComp n | n <= 0 = (clone 0 (n*(-1)) ++ xs, xss)
            | otherwise = (xs, clone 0 n ++ xss)


-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero [] = []
removeZero (0:xs) = removeZero xs
removeZero xs = xs


-- Question 19
-- You will be writing 3 functions in this code. This problem is hard. 


-- `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

bigAdd :: BigInt -> BigInt -> BigInt
bigAdd xs ys = listReverse (bigAddHelp 0 (listReverse xs) (listReverse ys))

bigAddHelp :: Int -> BigInt -> BigInt -> BigInt
bigAddHelp 0 xs [] = xs
bigAddHelp 0 [] ys = ys
bigAddHelp carry [] [] = [carry]

bigAddHelp carry (x:xs) [] = if (carry + x < 10)
    then (carry + x):bigAddHelp 0 xs []
    else (carry + x - 10):bigAddHelp 1 xs []

bigAddHelp carry [] (y:ys) = if (carry + y < 10)
    then (carry + y):bigAddHelp 0 [] ys
    else (carry + y - 10):bigAddHelp 1 [] ys

bigAddHelp carry (x:xs) (y:ys) = let total = x + y + carry
    in if total < 10
        then total:bigAddHelp 0 xs ys
        else (total - 10):bigAddHelp 1 xs ys


-- `mulByDigit i n` returns the result of multiplying
--  the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]

mulByDigit :: Int -> BigInt -> BigInt
mulByDigit x xs = listReverse (mulByDigHelp x 0 (listReverse xs))

mulByDigHelp :: Int -> Int -> BigInt -> BigInt
mulByDigHelp _ 0 [] = []
mulByDigHelp _ carry [] = if (carry >= 0) then [carry] else []

mulByDigHelp x1 carry (x:xs) = 
    let prod = x1 * x + carry
        num = prod `mod` 10
        carry1 = prod `div` 10
    in num:mulByDigHelp x1 carry1 xs


-- `bigMul n1 n2` returns the `BigInt` representing the 
--  product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

bigMul :: BigInt -> BigInt -> BigInt
bigMul x y = [0]

-----------------------------------------------------------------------
