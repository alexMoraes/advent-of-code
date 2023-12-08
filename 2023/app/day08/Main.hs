{-# LANGUAGE OverloadedStrings #-}
import Paths_adventofcode2023 (getDataFileName)
import qualified Data.Text as T
import Data.Map (fromList, findWithDefault, Map, keys)
import Data.List (scanl', findIndex)
import Data.Maybe (fromMaybe)

parseLine :: String -> (String, (String, String))
parseLine l = do
  let parts = T.splitOn " = " . T.pack $ l
  let node = T.unpack . head $ parts
  let connections =  map T.unpack . T.splitOn ", " . T.replace ")" "" . T.replace "(" "" . last $ parts
  (node, (head connections, last connections))

parseConnections :: String -> Map String (String, String)
parseConnections = Data.Map.fromList . map parseLine . lines

parseInput :: String -> (String, Map String (String, String))
parseInput ls = do
  let parts = map T.unpack . T.splitOn "\n\n" . T.pack $ ls
  (head parts, parseConnections . last $ parts)

follow :: Char -> (String, String) -> String
follow i (l,r) = case  i of
  'L' -> l
  _ -> r

navigate :: Map String (String, String) -> String -> (String -> Bool) -> String -> Int
navigate m is f s =
  fromMaybe (-1) .
  findIndex f .
  scanl' (\n i -> follow i . findWithDefault ("", "") n $ m) s . cycle $ is

getSteps :: String -> Int
getSteps ls = do
  let (is,m) = parseInput ls
  navigate m is (== "ZZZ") "AAA"

ghostSteps :: String -> Int
ghostSteps ls = do
  let (is,m) = parseInput ls
  let startingPoints = filter (\n -> last n == 'A') . keys $ m
  foldr (lcm . navigate m is (\n -> last n == 'Z')) 1 startingPoints

main :: IO ()
main = do
  path <- getDataFileName "day08\\input.txt"
  ls <- readFile path
  print ("First solution: " ++ show (getSteps ls))
  print ("Second solution: " ++ show (ghostSteps ls))
