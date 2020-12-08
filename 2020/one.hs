-- advent of code 2020 day 1 implementation in haskell

import Data.List (scanl')
import System.Environment (getArgs)

-- solution to part one
findOne :: [Int] -> Maybe Int
findOne l = let (_, _, r) = last . take takeLength $ scanl' handler (0, 0, Nothing) (cycle l) in r
  where
    arrayLength = length l
    takeLength = ((arrayLength * arrayLength) `div` 2) + 1
    handler s@(_, _, Just _) _ = s
    handler (_, 0, Nothing) y = (y, arrayLength, Nothing)
    handler (x, r, Nothing) y =
      if 2020 == (x + y)
        then (0, 0, Just (x * y))
        else (x, r - 1, Nothing)

-- solution to part two
findTwo :: [Int] -> [Int]
findTwo l = [mul x | x <- subsequencesOfSize 3 l, (sum x) == 2020]
  where
    mul (a : b : c : []) = a * b * c

-- helper for getting subsequences of a certain size
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

-- runner
main = do
  strArgs <- getArgs
  let args = map read strArgs
   in putStrLn $
        "one: " ++ (show . findOne args) ++ "\n"
          ++ "two: "
          ++ (show . findTwo args)
