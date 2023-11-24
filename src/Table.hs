module Table where

import Data.List
import Control.Monad (liftM)

data TableOption = TableOption {
    cellwidth ::Int,
    rowsplit :: String,
    colsplit :: String
}

default_option = TableOption 15 "\n" " | "
emptyCell = "" :: String

padding :: Int -> String -> String
padding n str = str ++ (replicate (n - length str) ' ')

concatWith :: String -> [String] -> String
concatWith c xs = foldl (\acc x -> acc ++ c ++ x) "" xs ++ c

toStringTable :: (Show a) => [[a]] -> [[String]]
toStringTable = map (map show)

makeTable :: [a] -> [b] -> [[(a,b)]]
makeTable xs ys = map (\x -> map (\y -> (x, y)) ys) xs

showTable :: (Show a, Show b, Show c) => TableOption -> [c] -> [b] -> [[a]] -> String
showTable (TableOption cw rs cs) left header datas = concatWith rs col_concat
    where table_with_left = zipWith (\x y -> [show x] ++ y) left (toStringTable datas)
          complete_table = [emptyCell:map show header] ++ table_with_left
          col_concat = liftM (concatWith cs) ((liftM.liftM) (padding cw) complete_table)

test_table = showTable default_option ["a", "b", "c"] [1, 2, 3] [[1,2,3], [4,5,6], [7,8,9]]
