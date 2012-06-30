-- Author : Afroz Mohiuddin

-- Knuth Morris Pratt Algorithm Implementation --

import ArrayUtils(changeElementAt, insertValue, stringToArray)
import Data.Array

-- Helper function to |prefixPosition|. Takes the string that we want to
-- construct the table for |needle|, position at which we are trying to extend
-- |forThis|, current position where we are searching |currentAt|, an input
-- array that has all the positions filled till |forThis - 1|, |arrayTillNow|.
prefixPositionHelper :: String -> Int -> Int -> Array Int Int -> Array Int Int
prefixPositionHelper needle forThis currentAt arrayTillNow
  | (forThis == 0) || (currentAt == (-1)) = insertValue arrayTillNow (-1)
  | (needle !! ((arrayTillNow ! currentAt) + 1)) == (needle !! forThis) = insertValue arrayTillNow ((arrayTillNow ! currentAt) + 1)
  | otherwise  = prefixPositionHelper needle forThis (arrayTillNow ! currentAt) arrayTillNow

-- |prefixPosition| returns an array, such that array ! i is the *index* of the
-- largest proper prefix that is also a suffix. Also, we prepend a sentinel array ! (-1) = -1
prefixPosition needle = foldl f (array ((-1),(-1)) [((-1), (-1))]) [0 .. length needle - 1]
  where f assocArray indexInArray = prefixPositionHelper needle indexInArray (indexInArray - 1) assocArray

kmpSearchHelper :: Array Int Char -> Array Int Char -> Int -> Int -> Array Int Int -> [Int] -> [Int]
kmpSearchHelper needle haystack posnHaystack posnNeedle prefixArray listMatches
-- Have we overshot the needle? Shouldn't happen.
  | posnNeedle > maxNeedle = listMatches
-- If we match from here, do we exhaust the haystack itself? If so, return.
  | (posnHaystack + maxNeedle) > maxHaystack = listMatches
-- Both characters match, and end of needle.
  | (needleChar == haystackChar) && (posnNeedle == maxNeedle) = kmpSearchHelper needle haystack (posnHaystack + posnNeedle + 1) 0 prefixArray (posnHaystack:listMatches)
-- Both characters match, not the end of the needle.
  | needleChar == haystackChar = kmpSearchHelper needle haystack posnHaystack (posnNeedle + 1) prefixArray listMatches
-- Start of the needle itself doesn't match the character, move ahead.
  | posnNeedle == 0 = kmpSearchHelper needle haystack (posnHaystack + 1) 0 prefixArray listMatches
-- Normal recursion.
  | otherwise = kmpSearchHelper needle haystack (posnHaystack + posnNeedle - 1 - prefPosn) (prefPosn + 1) prefixArray listMatches
  where maxNeedle = snd $ bounds needle
        maxHaystack = snd $ bounds haystack
        needleChar = needle ! posnNeedle
        haystackChar = haystack ! (posnHaystack + posnNeedle)
        prefPosn = prefixArray ! (posnNeedle - 1)

kmpSearch :: String -> String -> [Int]
kmpSearch needle haystack = kmpSearchHelper (stringToArray needle) (stringToArray haystack) 0 0 (prefixPosition needle) []