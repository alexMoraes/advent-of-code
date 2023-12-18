{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Lib (readLines)
import Data.Map (fromList, lookup, Map)
import Data.List (find)
import Prelude hiding (traverse, lookup)
import Data.Maybe (fromMaybe)
import Data.HashSet ( Set, fromList, member )

data Origin = North | East | South | West
  deriving (Show, Eq, Ord)

data RayStatus = Inner | Outer | PipeInner Char | PipeOuter Char
  deriving (Show, Eq, Ord)

type PipeMap = Map (Int, Int) Char

next :: Char -> Origin -> Int -> Int -> Maybe (Int, Int, Origin)
next pipe direction row column = case (pipe, direction) of
  ('|', North)  -> Just (row + 1, column, North)
  ('|', South)  -> Just (row - 1, column, South)
  ('|', _)      -> Nothing
  ('-', West)   -> Just (row, column + 1, West)
  ('-', East)   -> Just (row, column - 1, East)
  ('-', _)      -> Nothing
  ('L', North)  -> Just (row, column + 1, West)
  ('L', East)   -> Just (row - 1, column, South)
  ('L', _)      -> Nothing
  ('J', North)  -> Just (row, column - 1, East)
  ('J', West)   -> Just (row - 1, column, South)
  ('J', _)      -> Nothing
  ('7', South)  -> Just (row, column - 1, East)
  ('7', West)   -> Just (row + 1, column, North)
  ('7', _)      -> Nothing
  ('F', East)   -> Just (row + 1, column, North)
  ('F', South)  -> Just (row, column + 1, West)
  ('F', _)      -> Nothing
  ('.', _)      -> Nothing
  ('S', _)      -> Nothing
  _             -> Nothing

parseLine :: Int -> String -> [((Int, Int), Char)]
parseLine rowIndex = zipWith (\columnIndex pipe -> ((rowIndex, columnIndex), pipe)) [0..]

parseInput :: [String] -> [((Int, Int), Char)]
parseInput = concat . zipWith parseLine [0..]

mapPipes :: [String] -> PipeMap
mapPipes = Data.Map.fromList . parseInput

findStartingPosition :: [String] -> (Int, Int)
findStartingPosition = maybe (-1, -1) fst . find (\(_, c) -> c == 'S') . parseInput

getNextPipe :: (PipeMap, Char, Origin, Int, Int) -> (PipeMap, Char, Origin, Int, Int)
getNextPipe (pipes, pipe, direction, row, column) = do
  let (r, c, d) = fromMaybe (-1, -1, North) . next pipe direction row $ column
  maybe (pipes, '.', North, -1, -1) (\p -> (pipes, p, d, r, c)) . lookup (r, c) $ pipes

traverse :: PipeMap -> (Char, Origin, Int, Int) -> [(Char, Origin, Int, Int)]
traverse pipes (startingPipe, startingDirection, startingRow, startingColumn) =
  takeWhile (\(_, _, r, c) -> r /= -1 && c /= -1) .
  map (\(_, p, d, r, c) -> (p, d, r, c)) .
  iterate getNextPipe $
  (pipes, startingPipe, startingDirection, startingRow, startingColumn)

findLoop :: PipeMap -> Int -> Int -> [(Int, Int, Char)]
findLoop pipes startingRow startingColumn =
  maybe [] (map (\(p, _, r, c) -> (r, c, p))) .
  find (\loop -> head (map (\(_, d, _, _) -> d) loop) == last (map (\(_, d, _, _) -> d) loop)) .
  filter (\loop -> last (map (\(c, _, _, _) -> c) loop) == 'S') .
  map (traverse pipes) $
  [
    ('|', North, startingRow, startingColumn),
    ('-', West, startingRow, startingColumn),
    ('L', North, startingRow, startingColumn),
    ('J', North, startingRow, startingColumn),
    ('7', West, startingRow, startingColumn),
    ('F', South, startingRow, startingColumn)
  ]

findFarthestPoint :: Foldable t => t a -> Int
findFarthestPoint loop = length loop `div` 2

asd :: Set (Int, Int) -> Char -> ((Int, Int), Char) -> (RayStatus, Int) -> (RayStatus, Int)
asd l s' ((r, c), p) (s, t) | (r, c) `member` l = case (s, p) of
  (_, 'S')              -> asd l s' ((r, c), s') (s, t)

  (Outer, p')           -> (PipeOuter p', t)

  (PipeOuter '|', p')   -> (PipeInner p', t)

  (PipeOuter 'L', 'J')  -> (PipeInner 'J', t)
  (PipeOuter 'L', '7')  -> (PipeOuter '7', t)
  (PipeOuter 'L', _)    -> (PipeOuter 'L', t)

  (PipeOuter 'J', p')   -> (PipeInner p', t)
  (PipeOuter '7', p')   -> (PipeInner p', t)

  (PipeOuter 'F', '7')  -> (PipeInner '7', t)
  (PipeOuter 'F', 'J')  -> (PipeOuter 'J', t)
  (PipeOuter 'F', _)    -> (PipeOuter 'F', t)

  (PipeOuter p', _)      -> (PipeOuter p', t)

  (Inner, p')           -> (PipeInner p', t)

  (PipeInner '|', p')   -> (PipeOuter p', t)

  (PipeInner 'L', 'J')  -> (PipeOuter 'J', t)
  (PipeInner 'L', '7')  -> (PipeInner '7', t)
  (PipeInner 'L', _)    -> (PipeInner 'L', t)

  (PipeInner 'J', p')   -> (PipeOuter p', t)
  (PipeInner '7', p')   -> (PipeOuter p', t)

  (PipeInner 'F', '7')  -> (PipeOuter '7', t)
  (PipeInner 'F', 'J')  -> (PipeInner 'J', t)
  (PipeInner 'F', _)    -> (PipeInner 'F', t)

  (PipeInner p', _)      -> (PipeInner p', t)

asd _ _ _ (s, t) = case s of
  Outer -> (Outer, t)
  PipeOuter _ -> (Inner, t + 1)
  Inner -> (Inner, t + 1)
  PipeInner _ -> (Outer, t)

countInnerPlaces :: [(Int, Int, Char)] -> Char -> [String] -> Int
countInnerPlaces loop startingPipe ls = do
  let places = parseInput ls
  let l = Data.HashSet.fromList . map (\(r, c, _) -> (r, c)) $ loop
  snd . foldl (flip (asd l startingPipe)) (Outer, 0) $ places

main :: IO ()
main = do
  ls <- readLines "day10\\input.txt"
  let pipeMap = mapPipes ls
  let (startingRow, startingColumn) = findStartingPosition ls
  let loop = findLoop pipeMap startingRow startingColumn
  let (_, _, startingPipe) =  head loop
  print ("First solution: " ++ (show . findFarthestPoint $ loop))
  print ("Second solution: " ++ (show . countInnerPlaces loop startingPipe $ ls))
