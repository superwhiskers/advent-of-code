-- advent of code day four implementation in haskell

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Ix (inRange)
import Data.List.Split (splitOn, splitOneOf)
import Data.Set ((\\))
import qualified Data.Set as Set
import System.Environment (getArgs)

-- convert the textual format of the passports into a list
stringToPassportFieldList :: String -> [[String]]
stringToPassportFieldList = removeEmpty . splitFields . splitEntries
  where
    removeEmpty = map $ filter (/= "")
    splitFields = map $ splitOneOf "\n "
    splitEntries = splitOn "\n\n"

-- parse a passport field into a key, value tuple
parseField :: String -> (String, String)
parseField = intoTuple . splitOn ":"
  where
    intoTuple (k : v : []) = (k, v)

-- possible passport field names
passportFieldNames :: Set.Set String
passportFieldNames = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

-- hexadecimal digits
hexDigits :: String
hexDigits = "0123456789abcdef"

-- eye colors
eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- get the missing fields in a passport entry
getMissingFields :: [(String, String)] -> Set.Set String
getMissingFields = removeEmpty . subtractFromKnown . createSetFromFields
  where
    removeEmpty = (\\ Set.fromList ["cid"])
    subtractFromKnown = (passportFieldNames \\)
    createSetFromFields = Set.fromList . (map fst)

-- validate the contents of the fields in a passport
validateFields :: [(String, String)] -> Bool
validateFields fields =
  if (foldr optionalBoolAcc 0 $ map validate fields) < 7
    then False
    else True
  where
    optionalBoolAcc m a = case m of
      Just True -> a + 1
      Just False -> a
      Nothing -> a
    validate (name, value) = case name of
      "byr" -> Just $ validateBirthYear value
      "iyr" -> Just $ validateIssueYear value
      "eyr" -> Just $ validateExpirationYear value
      "hgt" -> Just $ validateHeight value
      "hcl" -> Just $ validateHairColor value
      "ecl" -> Just $ validateEyeColor value
      "pid" -> Just $ validatePassportId value
      "cid" -> Nothing
      _ -> Nothing

    validateBirthYear = inRange (1920, 2002) . read
    validateIssueYear = inRange (2010, 2020) . read
    validateExpirationYear = inRange (2020, 2030) . read
    validateHeight value =
      let (suffix, rawNum) = join bimap reverse $ splitAt 2 $ reverse value
          num = read rawNum
       in case suffix of
            "cm" -> inRange (150, 193) num
            "in" -> inRange (59, 76) num
            _ -> False
    validateHairColor color = case color of
      '#' : color -> 
        if (length color) == 6
          then foldr (\c _ -> c `elem` hexDigits) False color
          else False
      _ -> False
    validateEyeColor = (`elem` eyeColors)
    validatePassportId pid =
      if (length pid) == 9
        then foldr (\d _ -> d `elem` ['0' .. '9']) False pid
        else False

-- runner
main = do
  fileName : [] <- getArgs
  rawPassports <- readFile fileName
  let passports = preparePassports rawPassports
      validPassports = foldr boolAcc 0 $ map (== Set.empty) $ map getMissingFields passports
      validPassportsPartTwo = foldr boolAcc 0 $ map validateFields passports
   in putStrLn $ "valid passports for part one: " ++ (show validPassports) ++ "\n" ++ "valid passports for part two: " ++ (show validPassportsPartTwo)
  where
    preparePassports = (map $ map parseField) . stringToPassportFieldList
    boolAcc b a = if b then a + 1 else a
