-- Author : Afroz Mohiuddin

module RabinKarp (rabinKarp) where

import Data.Char
import Slices(subString)

-- Stuff can be sped up by simply using Arrays instead of lists.

lastKnownTwoSidedPrime = 739397

-- Our hash function, interprets the string as a base $n$ number, for a large
-- prime number. We'll use 739397 which is the last two sided known prime.
calculateHash :: String -> Int
calculateHash input = let numList = map ord input
                      in foldl (\x y -> lastKnownTwoSidedPrime*x + y) 0 numList


-- Roll the hash, takes in the original hash, entering char, leaving char,
-- and the number of characters in the hash and returns the updated hash.
rollHash :: Int -> Char -> Char -> Int -> Int
rollHash originalHash enteringChar leavingChar lengthHash = ((originalHash - (power * (ord leavingChar))) * lastKnownTwoSidedPrime) + (ord enteringChar)
  where power = lastKnownTwoSidedPrime ^ (lengthHash - 1)

	-- Haystack - Needle - hash of needle  - position we are at - input hash - input positions - output positions
	rabinKarpHelper :: String -> String -> Int -> Int -> Int -> [Int] -> [Int]
	rabinKarpHelper [] _ _ _ _ _ = []
	rabinKarpHelper _ [] _ _ _ _ = []
	rabinKarpHelper haystack needle needleHash position inputHash inputPositions
	  | position + lenNeedle > length haystack = inputPositions -- Too less input left.
	  | inputHash /= needleHash = rabinKarpHelper haystack needle needleHash (position + 1) (rollHash inputHash (haystack !! (position + lenNeedle)) (haystack !! position) lenNeedle) inputPositions
	  | (inputHash == needleHash) && ((subString haystack position lenNeedle) == needle) = rabinKarpHelper haystack needle needleHash (position + lenNeedle) (calculateHash (subString haystack (position + lenNeedle) lenNeedle)) (position : inputPositions)
	  where lenNeedle = length needle

-- Takes in a string in which to find another string and return list of
-- positions where the second string is found in the first string.
--
-- We return non-overlapping matches, starting from the left, and the list of
-- positions returned is in descending order, i.e. last match's position is
-- listed first.
rabinKarp :: String -> String -> [Int]
rabinKarp haystack needle
  | length haystack < length needle  = []
  | otherwise                        = rabinKarpHelper haystack needle (calculateHash needle) 0 (calculateHash $ subString haystack 0 (length needle)) []



