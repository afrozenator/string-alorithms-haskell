-- Author : Afroz Mohiuddin

-- Primitive tests for testing the search algorithms.

module Main where

import RabinKarp(rabinKarp)
import KnuthMorrisPratt(knuthMorrisPratt)

testSearch :: (String -> String -> [Int]) -> [(String, String, [Int])] -> [(String, Bool)]
testSearch searchFunction suite = let listResults = map (\(a, b, c) -> (searchFunction a b) == c) suite
                                      testCases   = zipWith (\(a, b, c) x -> (a ++ "-->" ++ b, x)) suite listResults
                                  in filter (not.snd) testCases

testSearchPrintFailures searchFunction suite = mapM_ (putStrLn.fst) (testSearch searchFunction suite)

main = do
  let testSuite = [("a", "alina alina afroz alina", [22,18,12,10,6,4,0]), ("ali", "alina alina afroz alina", [18,6,0]), ("ababa", "abbabababbababbababaababa", [20, 15, 3])]
  putStrLn $ "Testing KnuthMorrisPratt"
  testSearchPrintFailures knuthMorrisPratt testSuite
  putStrLn $ "Testing RabinKarp"
  testSearchPrintFailures rabinKarp testSuite
