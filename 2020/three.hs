-- advent of code day three implementation in haskell

import Data.Ratio
import System.Environment (getArgs)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read as R

-- possible grid items
data GridItem = Open | Tree
  deriving (Eq)

instance Show GridItem where
  show Open = "."
  show Tree = "#"

instance Read GridItem where
  readListPrec = R.readListPrecDefault

  readPrec = R.readP_to_Prec $
    const $ do
      c <- P.get
      return $ case c of
        '.' -> Open
        '#' -> Tree

-- transform a string into a grid item array
stringToGridItemArray :: String -> [[GridItem]]
stringToGridItemArray s = map craftLine $ lines s
  where
    craftLine = map $ read . pure

-- prepare a cyclic grid from the seed array
makeInfiniteGrid :: [[GridItem]] -> [[GridItem]]
makeInfiniteGrid g = map cycle g

-- gather points on a grid from a point, a ratio, and a grid
gatherPoints :: (Int, Int) -> Ratio Int -> [[GridItem]] -> [(Int, Int)]
gatherPoints (x, y) s g = zip [x, (denominator s) + x ..] [y, (numerator s) + y .. length g - 1]

-- walk the grid based on the provided points
walkGrid :: [(Int, Int)] -> [[GridItem]] -> [GridItem]
walkGrid p g = foldr itemAcc [] p
  where
    itemAcc (x, y) a = (g !! y !! x) : a

-- runner
main = do
  fileName : [] <- getArgs
  rawGrid <- readFile fileName
  let grid = makeInfiniteGrid $ stringToGridItemArray rawGrid
      slopes = [(1 % 1), (1 % 3), (1 % 5), (1 % 7), (2 % 1)]
      pointSet = map ((flip $ gatherPoints (0, 0)) grid) slopes
      walkResults = map (foldr treeAcc 0) $ map ((flip walkGrid) grid) pointSet
   in putStrLn $ "results: " ++ (show $ zip slopes walkResults) ++ "\n" ++ "multiplied: " ++ (show $ foldr (*) 1 walkResults)
  where
    treeAcc i a = if i == Tree then a + 1 else a
