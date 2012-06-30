-- Author : Afroz Mohiuddin

module Slices(subString, fromPosnToEnd, fromStartToPosn) where

-- Substring of thisString, starting at posnStart and spanning len chars.
subString :: String -> Int -> Int -> String
subString thisString posnStart len = fst (splitAt len (snd (splitAt posnStart thisString)))

-- Substring of |string| from and including the index |posn| till the end.
fromPosnToEnd :: String -> Int -> String
fromPosnToEnd string posn = snd $ splitAt posn string

-- Substring of |string| from start till index |posn| excluding the index |posn|.
fromStartToPosn :: String -> Int -> String
fromStartToPosn string posn = fst $ splitAt posn string
