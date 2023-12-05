{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T (splitOn, pack, unpack, replace)
import Paths_adventofcode2023 (getDataFileName)
import Data.Foldable (Foldable(foldl'))
import Data.List (find)

splitSections :: String -> [String]
splitSections = map T.unpack . T.splitOn "\n\n" . T.pack

getSeeds :: [String] -> [Int]
getSeeds (h:_) = map read . words . T.unpack . T.replace "seeds: " "" . T.pack $ h
getSeeds _ = []

isMap :: Int -> [Int] -> Bool
isMap o [destStart, _ ,range] = o >= destStart && o < destStart + range
isMap _ _ = False

getInputSingle :: [Int] -> Int -> Int
getInputSingle [destStart, sourceStart, range] o | o >= destStart && o < destStart + range = sourceStart + (o - destStart)
getInputSingle _ o = o

getInput :: [[Int]] -> Int -> Int
getInput maps o = do
  case find (isMap o) maps of
    Just iMap -> getInputSingle iMap o
    Nothing  -> o

getSeedRanges :: [Int] -> [(Int, Int)]
getSeedRanges [start, range] = [(start, range)]
getSeedRanges (start:(range:t)) = (start, range) : getSeedRanges t
getSeedRanges _ = []

isValidSeed :: [Int] -> Int -> Bool
isValidSeed seedRanges seed = do
  let seedRange = find (\(start, range) -> seed >= start && seed < start + range) . getSeedRanges $ seedRanges
  case seedRange of
    Just _ -> True
    Nothing -> False

getMap :: String -> [[Int]]
getMap (_:t) = (map (map read . words) . lines) t
getMap _ = []

getAllMaps :: [String] -> [[[Int]]]
getAllMaps = map getMap

foldInputs :: Int -> [[[Int]]] -> Int
foldInputs = foldr getInput

findSeed :: [Char] -> Int
findSeed ls = do
  let sections = splitSections ls
  let seeds = getSeeds sections
  let maps = getAllMaps sections
  case find (isValidSeed seeds) . map (`foldInputs` maps) $ [1..] of
    Just s -> s
    _ -> -1

------------------
-- Bad solution --
------------------

buildMapCondition :: [Int] -> (Int, Bool) -> (Int, Bool)
buildMapCondition [destStart, sourceStart, range] = do
  let mapSeed (s, False) | s >= sourceStart && s < (sourceStart + range) = (destStart + (s - sourceStart), True)
      mapSeed (s, match) = (s, match)
  mapSeed
buildMapCondition _ = \(s, match) -> (s, match)

buildMap :: String -> [(Int, Bool) -> (Int, Bool)]
buildMap = map ((buildMapCondition . map read) . words) . lines

mapFunction :: [Char] -> Int -> Int
mapFunction (_:t) s = fst . foldr (\f p -> f p) (s, False) . buildMap $ t
mapFunction _ _ = -1

getMaps :: [[Char]] -> [Int -> Int]
getMaps (_:t) = map mapFunction t
getMaps _ = []

getLocations :: [Int -> Int] -> [Int] -> [Int]
getLocations maps seeds = do
  map (\s -> foldl' (\m f -> f m) s maps) seeds

main :: IO ()
main = do
  path <- getDataFileName "day05\\input.txt"
  ls <- readFile path
  let sections = splitSections ls
  let maps = getMaps sections
  let seeds = getSeeds sections
  print ("First solution: " ++ show (minimum . getLocations maps $ seeds))
  print ("First solution: " ++ show (getLocations maps [findSeed ls]))
