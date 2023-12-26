{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import Lib (readLines)
import qualified Data.Text as T
import Data.List (intercalate)

isValidPartial :: [Int] -> String -> Bool
isValidPartial groups l = do
  let partialGroups = zip groups . snd . foldr (\s (g, acc) -> case (s, g) of
        ('.', g') | g' > 0 -> (0, g : acc)
        ('.', _) -> (0, acc)
        ('#', g') -> (g'+1, acc)
        _ -> (g, acc)) (0, []) . ('.' :) . takeWhile (/= '?') $ l
  let lastKnownSpring = last . takeWhile (/= '?') $ l
  let checkBuilding nextGroup '.' | uncurry (==) nextGroup = True
      checkBuilding nextGroup '#' | uncurry (>=) nextGroup = True
      checkBuilding _ _ = False
  let checkValid [] = True
      checkValid gs = (all (uncurry (==)) . init $ gs) && checkBuilding (last gs) lastKnownSpring
  checkValid partialGroups

isValid :: [Int] -> String -> Bool
isValid groups = (groups ==) . snd . foldr (\s (g, acc) -> case (s, g) of
  ('.', g') | g' > 0 -> (0, g : acc)
  ('.', _) -> (0, acc)
  ('#', g') -> (g'+1, acc)
  _ -> (g, acc)) (0, []) . ('.' :)

explore :: [Int] -> [Char] -> Int
explore groups springs | '?' `notElem` springs && isValid groups springs = 1
explore groups springs | '?' `notElem` springs && not (isValid groups springs) = 0
explore groups springs | '?' `elem` springs && not (isValidPartial groups springs) = 0
explore groups springs = do
  let left = takeWhile (/= '?') springs
  let right = dropWhile (/= '?') springs
  explore groups (left ++ ('.' : tail right)) + explore groups (left ++ ('#' : tail right))

parseLine :: String -> ([Int], String)
parseLine l = do
  let parts = T.splitOn " " . T.pack $ l
  let springs = T.unpack . head $ parts
  let groups = map (read . T.unpack) . T.splitOn "," . last $ parts
  (groups, springs)

totalCount :: [String] -> Int
totalCount = sum . map (uncurry explore . parseLine)

totalUnfolded :: Int -> [String] -> Int
totalUnfolded factor = sum . map (uncurry explore . (\(gs, ss) -> (concat . replicate factor $ gs, intercalate "?" . replicate factor $ ss)) . parseLine)

main :: IO ()
main = do
  ls <- readLines "day12\\input.txt"
  print ("First solution: " ++ (show . totalCount $ ls))
  print ("Second solution: " ++ (show . totalUnfolded 5 $ ls))
