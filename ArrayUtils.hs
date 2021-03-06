-- Author : Afroz Mohiuddin

module ArrayUtils(changeElementAt, insertValue, stringToArray) where

import Data.Array

stringToArray :: [e] -> Array Int e
stringToArray s = array (0, length s - 1) (zip [0..] s)

insertValue :: (Ix a, Num a) => Array a e -> e -> Array a e
insertValue arrayTillNow value = let oldBounds = bounds arrayTillNow
                                 in array (fst oldBounds, snd oldBounds + 1) (assocs arrayTillNow ++ [(snd oldBounds + 1, value)]) 

changeElementAt :: Ix i => i -> e -> Array i e -> Array i e
changeElementAt key value assocArray = assocArray // [(key, value)]

-- Helper Functions --

replaceKeyValue key value assocList = map (f key value) assocList

f key value x
 | fst x == key = (key, value)
 | otherwise    = x
