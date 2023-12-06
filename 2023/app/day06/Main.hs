{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Lib (readLines)
import GHC.Float (int2Float)
import Data.List (singleton)

delta :: Num a => a -> a -> a
delta b c = b*b - 4 * c

distance :: Num a => a -> a -> a
distance raceTime holdTime = raceTime * holdTime - holdTime * holdTime

adjustL :: (Num a, Eq a) => a -> a -> a -> a
adjustL raceTime raceRecord holdTime = case distance raceTime holdTime - raceRecord of
  0 -> holdTime + 1
  _ -> holdTime

adjustU :: (Num a, Eq a) => a -> a -> a -> a
adjustU raceTime raceRecord holdTime = case distance raceTime holdTime - raceRecord of
  0 -> holdTime - 1
  _ -> holdTime

holdTimes :: Int -> Int -> (Int, Int)
holdTimes raceTime raceRecord = do
  let d = sqrt (delta (int2Float raceTime) (int2Float raceRecord))
  let l = ceiling ((int2Float raceTime - d) / 2)
  let u = floor ((int2Float raceTime + d) / 2)
  (adjustL raceTime raceRecord l, adjustU raceTime raceRecord u)

parseLine :: String -> [Int]
parseLine = map read . words . last . map T.unpack . T.splitOn (T.pack ":") . T.pack

parseLineWithKerning :: String -> [Int]
parseLineWithKerning = singleton . read . T.unpack . T.replace (T.pack " ") (T.pack "") . last . T.splitOn (T.pack ":") . T.pack

parseInput :: (String -> [Int]) -> [String] -> [(Int, Int)]
parseInput f = do
  let combine (t:(h:_)) = zip t h
      combine _ = []
  combine . map f

getHoldRanges :: [(Int, Int)] -> [Int]
getHoldRanges = map ((\(l, u) -> u - l + 1) . uncurry holdTimes)

main :: IO ()
main = do
  ls <- readLines "day06\\input.txt"
  print ("First solution: " ++ show (product . getHoldRanges . parseInput parseLine $ ls))
  print ("Second solution: " ++ show (product . getHoldRanges . parseInput parseLineWithKerning $ ls))
