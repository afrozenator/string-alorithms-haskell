-- Author : Afroz Mohiuddin

-- Knuth Morris Pratt Algorithm Implementation --

import ArrayUtils(changeElementAt, insertValue)
import Data.Array

prefixPositionHelper :: String -> Int -> Int -> Array Int Int -> Array Int Int
prefixPositionHelper needle forThis currentAt arrayTillNow
  | forThis == 0 = array (0,0) [(0, (-1))]
  | currentAt == (-1) = insertValue arrayTillNow (-1)
  | (needle !! ((arrayTillNow ! currentAt) + 1)) == (needle !! forThis) = insertValue arrayTillNow ((arrayTillNow ! currentAt) + 1)
  | otherwise  = prefixPositionHelper needle forThis (arrayTillNow ! currentAt) arrayTillNow

prefixPosition needle = foldl f undefined [0 .. length needle - 1]
  where f assocArray indexInArray = prefixPositionHelper needle indexInArray (indexInArray - 1) assocArray