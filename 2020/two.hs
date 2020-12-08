-- advent of code day two implementation in haskell
-- aehusaoneuhaosnteuhnsoteahaoeus  

import Data.List.Split (splitOn, wordsBy)
import System.Environment (getArgs)

-- extracts the password policy from the first portion of a password entry
getPolicy :: String -> ([Int], Char)
getPolicy rawPolicy = (range, char)
  where
    r : (char : []) : [] = wordsBy (== ' ') rawPolicy
    range = map read $ wordsBy (== '-') r

-- an implementation of a validator for part one
validateOne :: [Int] -> Char -> String -> Bool
validateOne (s : e : []) char password = elem (foldr check 0 password) [s .. e]
  where
    check c a =
      if c == char
        then a + 1
        else a

-- an implementation of a validator for part two
validateTwo :: [Int] -> Char -> String -> Bool
validateTwo range char password =
  if (foldr check 0 range) == 1
    then True
    else False
  where
    check i a =
      if (password !! (i - 1)) == char
        then a + 1
        else a

-- validates a password entry
validateEntry :: ([Int] -> Char -> String -> Bool) -> String -> Bool
validateEntry validator entry = validator range char password
  where
    criteria : password : [] = splitOn ": " entry
    (range, char) = getPolicy criteria

-- runner
main = do
  file : [] <- getArgs
  rawEntries <- readFile file
  let entries = lines rawEntries
   in putStrLn $ "one: " ++ (runValidator validateOne entries) ++ "\n" ++ "two: " ++ (runValidator validateTwo entries)
  where
    accBools b a =
      if b
        then a + 1
        else a
    runValidator v e = show . foldr accBools 0 $ map (validateEntry v) e
