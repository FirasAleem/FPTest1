-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

checkParity :: String -> Bool
checkParity [] = False
checkParity bit = (evenOnes bit '1') && ((length bit) `mod` 8 == 0)

evenOnes :: String -> Char -> Bool
evenOnes bit '1' = (length (filter (=='1') bit)) `mod` 2 == 0
evenOnes bit _ = False

{- Question 2 -}

substitution :: String -> String -> String
--substitution (p:ps) key = if (isUpper p && isLetter p) then (key !! (charLabel p)) : (substitution ps key) else (toLower (key !! (charLabel p))) : (substitution ps key)
--substitution (p:ps) key = if not (isLetter p) then p : (substitution ps key) else substitution (p:ps) key

substitution [] [] = []
substitution [] (_) = []
substitution (p:ps) key | isLetter p = if (isUpper p) then (key !! (charLabel p)) : (substitution ps key) else (toLower (key !! (charLabel p))) : (substitution ps key)
                        | otherwise = p : (substitution ps key)

--charLabel p is index of key we need to use
--(key !! charLabel p) is the letter we are replacing p by 
--Upper or lower case

--substitution :: String -> String -> String
--substitution (p:ps) ('A':ks) = if (toUpper p) == p then "A" ++ (substitution ps (ks)) else "a" ++ (substitution ps (ks))
--substitution (p:ps) ('B':ks) = if (toUpper p) == p then "B" ++ (substitution ps (ks)) else "b" ++ (substitution ps (ks))
--substitution (p:ps) ('C':ks) = if (toUpper p) == p then "C" ++ (substitution ps (ks)) else "c" ++ (substitution ps (ks))
--substitution (p:ps) ('D':ks) = if (toUpper p) == p then "D" ++ (substitution ps (ks)) else "d" ++ (substitution ps (ks))
--substitution (p:ps) ('E':ks) = if (toUpper p) == p then "E" ++ (substitution ps (ks)) else "e" ++ (substitution ps (ks))
--substitution (p:ps) ('F':ks) = if (toUpper p) == p then "F" ++ (substitution ps (ks)) else "f" ++ (substitution ps (ks))
--substitution (p:ps) ('G':ks) = if (toUpper p) == p then "G" ++ (substitution ps (ks)) else "g" ++ (substitution ps (ks))
--substitution (p:ps) ('H':ks) = if (toUpper p) == p then "H" ++ (substitution ps (ks)) else "h" ++ (substitution ps (ks))
--substitution (p:ps) ('I':ks) = if (toUpper p) == p then "I" ++ (substitution ps (ks)) else "i" ++ (substitution ps (ks))
--substitution (p:ps) ('J':ks) = if (toUpper p) == p then "J" ++ (substitution ps (ks)) else "j" ++ (substitution ps (ks))
--substitution (p:ps) ('K':ks) = if (toUpper p) == p then "K" ++ (substitution ps (ks)) else "k" ++ (substitution ps (ks))
--substitution (p:ps) ('L':ks) = if (toUpper p) == p then "L" ++ (substitution ps (ks)) else "l" ++ (substitution ps (ks))
--substitution (p:ps) ('M':ks) = if (toUpper p) == p then "M" ++ (substitution ps (ks)) else "m" ++ (substitution ps (ks))
--substitution (p:ps) ('N':ks) = if (toUpper p) == p then "N" ++ (substitution ps (ks)) else "n" ++ (substitution ps (ks))
--substitution (p:ps) ('O':ks) = if (toUpper p) == p then "O" ++ (substitution ps (ks)) else "o" ++ (substitution ps (ks))
--substitution (p:ps) ('P':ks) = if (toUpper p) == p then "P" ++ (substitution ps (ks)) else "p" ++ (substitution ps (ks))
--substitution (p:ps) ('Q':ks) = if (toUpper p) == p then "Q" ++ (substitution ps (ks)) else "q" ++ (substitution ps (ks))
--substitution (p:ps) ('R':ks) = if (toUpper p) == p then "R" ++ (substitution ps (ks)) else "r" ++ (substitution ps (ks))
--substitution (p:ps) ('S':ks) = if (toUpper p) == p then "S" ++ (substitution ps (ks)) else "s" ++ (substitution ps (ks))
--substitution (p:ps) ('T':ks) = if (toUpper p) == p then "T" ++ (substitution ps (ks)) else "t" ++ (substitution ps (ks))
--substitution (p:ps) ('U':ks) = if (toUpper p) == p then "U" ++ (substitution ps (ks)) else "u" ++ (substitution ps (ks))
--substitution (p:ps) ('V':ks) = if (toUpper p) == p then "V" ++ (substitution ps (ks)) else "v" ++ (substitution ps (ks))
--substitution (p:ps) ('W':ks) = if (toUpper p) == p then "W" ++ (substitution ps (ks)) else "w" ++ (substitution ps (ks))
--substitution (p:ps) ('X':ks) = if (toUpper p) == p then "X" ++ (substitution ps (ks)) else "x" ++ (substitution ps (ks))
--substitution (p:ps) ('Y':ks) = if (toUpper p) == p then "Y" ++ (substitution ps (ks)) else "y" ++ (substitution ps (ks))
--substitution (p:ps) ('Z':ks) = if (toUpper p) == p then "Z" ++ (substitution ps (ks)) else "z" ++ (substitution ps (ks))
--substitution (p:ps) (k) = if not(isLetter p) then p : (substitution ps k) else (substitution ps k)
--substitution [] [] = []
--substitution [] _ = []


--charLabelKeepCase :: Char -> Int
--charLabelKeepCase char =  ord (char) - ord 'A'


--charLabelList :: String -> [Int]
--charLabelList string = map charLabelKeepCase string

{- Question 3 -}

largestPrimeBetween :: Int -> Int
--largestPrimeBetween num = head(reverse [p | p <- [num..(2*num)], not (p `mod` 2 == 0), isPrime p])
largestPrimeBetween 0 = error "No primes"
largestPrimeBetween 1 = error "1 is not prime"
largestPrimeBetween num = [p | p <- reverse [num..(2*num)], not (p `mod` 2 == 0), isPrime p] !! 0


nextPrime :: Int -> Int
nextPrime num = [p | p <- [num+1..(2*num)], not (p `mod` 2 == 0), isPrime p] !! 0

prevPrime :: Int -> Int
prevPrime num = [p | p <- (reverse [1..(num-1)]), not (p `mod` 2 == 0), isPrime p] !! 0

strongPrimes :: Int -> [Int]
strongPrimes n = take n [ p | p <- [11..], isPrime p, ((nextPrime p) + (prevPrime p)) `div` 2 < p ]


{- Question 4 -}


command :: (Direction, Int) -> (Int , Int) -> (Int , Int)
command (MoveLeft, left) (x,y) = (x-left,y)
command (MoveRight, right) (x,y) = (x+right,y)
command (MoveUp, up) (x,y) = (x,y+up)
command (MoveDown, down) (x,y) = (x,y-down)


executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] x = x
executeCommands (c:cs) startPoint = executeCommands cs (command c startPoint) 

{- Question 5 -}

atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange amount denoms = atmChange' amount (reverse denoms)


--d:ds is denomsList
atmChange' :: Int -> [Int] -> [(Int, Int)]
--atmChange' 0 _ = []
atmChange' _ [] = []
atmChange' amount (d:ds) = [(d, (numberOfBills amount d))] ++ atmChange' (remainingAmount amount d) ds 

numberOfBills :: Int -> Int -> Int
numberOfBills amount denom = amount `div` denom


remainingAmount :: Int -> Int -> Int
remainingAmount amount denom = amount - ((numberOfBills amount denom)*denom)


--atmChange amount denomsList = [(x,y) | x <- denomsList, y<-]
