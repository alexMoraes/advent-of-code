{-# LANGUAGE OverloadedStrings #-}
import Data.List (find, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Paths_adventofcode2023 (getDataFileName)

isReflection :: Int -> String -> Bool
isReflection n l = all (uncurry (==)) . zip (reverse . take n $ l) $ drop n l

findReflection :: [String] -> Maybe Int
findReflection ls = do
  let getMaxLen [] = 0
      getMaxLen (h:_) = length h
  let maxLen = getMaxLen ls
  find (\c -> all (isReflection c) ls) [1..maxLen-1]

summarizePattern :: [String] -> Int
summarizePattern ls = case findReflection ls of
  Just s -> s
  Nothing -> (100 *) . fromMaybe 0 . findReflection . transpose $ ls

summarize :: [[String]] -> Int
summarize = sum . map summarizePattern

main :: IO ()
main = do
  path <- getDataFileName "day13\\input.txt"
  contents <- readFile path
  let patterns = (map (lines . T.unpack) . T.splitOn "\n\n" . T.pack) contents
  print ("First solution: " ++ (show . summarize $ patterns))
