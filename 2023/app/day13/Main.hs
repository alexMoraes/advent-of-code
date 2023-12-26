{-# LANGUAGE OverloadedStrings #-}
import Data.List (find, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Paths_adventofcode2023 (getDataFileName)

countDiffs :: Int -> String -> Int
countDiffs n l = length . filter (uncurry (/=)) . zip (reverse . take n $ l) $ drop n l

findReflection :: Int -> [String] -> Maybe Int
findReflection smudges ls = do
  let getMaxLen [] = 0
      getMaxLen (h:_) = length h
  let maxLen = getMaxLen ls
  find (\c -> (smudges ==) . sum . map (countDiffs c) $ ls) [1..maxLen-1]

summarizePattern :: Int -> [String] -> Int
summarizePattern smudges ls = case findReflection smudges ls of
  Just s -> s
  Nothing -> (100 *) . fromMaybe 0 . findReflection smudges . transpose $ ls

summarize :: Int -> [[String]] -> Int
summarize smudges = sum . map (summarizePattern smudges)

main :: IO ()
main = do
  path <- getDataFileName "day13\\input.txt"
  contents <- readFile path
  let patterns = (map (lines . T.unpack) . T.splitOn "\n\n" . T.pack) contents
  print ("First solution: " ++ (show . summarize 0 $ patterns))
  print ("Second solution: " ++ (show . summarize 1 $ patterns))
