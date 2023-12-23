{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Lib (readLines)
import Data.List (transpose, scanl')
import Data.Map (fromList, lookup, Map)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

combinations :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
combinations ns = concatMap (\(i, e) -> map (\e' -> (e, e')) . drop i $ ns) . zip [1..] $ ns

remapIndex :: Int -> Int -> String -> Int
remapIndex _ i l | '#' `elem` l = i
remapIndex n i _ = i + n

remapTwo :: Int -> String -> Int
remapTwo = remapIndex 1

remapMillion :: Int -> String -> Int
remapMillion = remapIndex 999999

remapIndexes :: (Int -> String -> Int) -> [String] -> Map Int Int
remapIndexes remap = fromList . tail . scanl' (\(_, i') (i, l) -> (i, remap (i' + 1) l)) (-1, -1) . zip [0..]

remapIndexesTwo :: [String] -> Map Int Int
remapIndexesTwo = remapIndexes remapTwo

remapIndexesMillion :: [String] -> Map Int Int
remapIndexesMillion = remapIndexes remapMillion

parseLine :: Map Int Int -> Int -> String -> [(Int, Int)]
parseLine columnMap r = map (\(c, _) -> (r, fromMaybe c . lookup c $ columnMap)) . filter (\(_, s) -> s /= '.') . zip [0..]

parseInput :: Map Int Int -> Map Int Int -> [String] -> [(Int, Int)]
parseInput rowMap columnMap = concat . zipWith (parseLine columnMap) (map (\r -> fromMaybe r . lookup r $ rowMap) [0..])

calculateDistance :: (Int, Int) -> (Int, Int) -> Int
calculateDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

distances :: [(Int, Int)] -> [Int]
distances = map (uncurry calculateDistance) . combinations

main :: IO ()
main = do
  ls <- readLines "day11\\input.txt"
  let expandTwoRowMap = remapIndexesTwo ls
  let expandTwoColumnMap = remapIndexesTwo . transpose $ ls
  let expandMillionRowMap = remapIndexesMillion ls
  let expandMillionColumnMap = remapIndexesMillion . transpose $ ls
  print "---"
  print (sum . distances . parseInput expandTwoRowMap expandTwoColumnMap $ ls)
  print (sum . distances . parseInput expandMillionRowMap expandMillionColumnMap $ ls)
