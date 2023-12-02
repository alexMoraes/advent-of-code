import Control.Monad ()
import Paths_adventofcode2023 (getDataFileName)
import qualified Data.Text as T (splitOn, pack, unpack, Text, replace)

readLines :: FilePath -> IO [String]
readLines fileName = do
  path <- getDataFileName fileName
  contents <- readFile path
  pure $ lines contents

data Draw = Draw {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Show)

data Game = Game {
  gameId :: Int,
  draws :: [Draw]
} deriving (Show)

splitId :: String -> [T.Text]
splitId = T.splitOn (T.pack ":") . T.pack

splitDraws :: T.Text -> [T.Text]
splitDraws = T.splitOn (T.pack ";")

parseGameId :: [T.Text] -> Int
parseGameId [] = 0
parseGameId (h:_) = read . T.unpack . T.replace (T.pack "Game ") (T.pack "") $ h

parseDraws :: [T.Text] -> [Draw]
parseDraws [] = []
parseDraws [_] = []
parseDraws (_:(s:_)) = map parseDraw . splitDraws $ s

parseGame :: String -> Game
parseGame l = do
  let parts = splitId l
  Game
    (parseGameId parts)
    (parseDraws parts)

filterColor :: String -> [String] -> Bool
filterColor _ [] = False
filterColor _ [_] = False
filterColor color [_, c] = c == color
filterColor color (h1:(h2:_)) = filterColor color [h1,h2]

parseColorCount :: [[String]] -> Int
parseColorCount [] = 0
parseColorCount ([]:_) = 0
parseColorCount ((c:_):_) = read c :: Int

parseDraw :: T.Text -> Draw
parseDraw s = do
  let colors = map (words . T.unpack) (T.splitOn (T.pack ",") s)
  Draw
    (parseColorCount . filter (filterColor "red") $ colors)
    (parseColorCount . filter (filterColor "green") $ colors)
    (parseColorCount . filter (filterColor "blue") $ colors)

isOverdraw :: Draw -> Bool
isOverdraw draw = do
  let redCubes = 12
  let greenCubes = 13
  let blueCubes = 14
  red draw > redCubes || green draw > greenCubes || blue draw > blueCubes

isValid :: Game -> Bool
isValid = not . any isOverdraw . draws

sumImpossibleGames :: [Game] -> Int
sumImpossibleGames = sum . map gameId . filter isValid

getMinimumSet :: Game -> Draw
getMinimumSet = foldr (\draw minDraw -> Draw (max (red draw) (red minDraw)) (max (green draw) (green minDraw)) (max (blue draw) (blue minDraw))) (Draw 0 0 0)
  . draws

getSetPower :: Game -> Int
getSetPower game = do
  let minimumSet = getMinimumSet game
  red minimumSet * green minimumSet * blue minimumSet

sumSetPowers :: [Game] -> Int
sumSetPowers = sum . map getSetPower

main :: IO ()
main = do
  ls <- readLines "day02\\input.txt"
  let games = map parseGame ls
  print ("Part one: " ++ show (sumImpossibleGames games))
  print ("Part two: " ++ show (sumSetPowers games))
