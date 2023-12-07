{-# LANGUAGE InstanceSigs #-}
import Lib (readLines)
import Data.List (sort)
import Data.Map (fromListWith, toList)

data Hand cards = FiveOfKind [Int]
  | FourOfKind [Int]
  | FullHouse [Int]
  | ThreeOfKind [Int]
  | TwoPair [Int]
  | OnePair [Int]
  | HighCard [Int]
  | InvalidHand
  deriving (Show, Eq)

instance Ord (Hand cards) where
  compare :: Hand cards -> Hand cards -> Ordering
  compare (FiveOfKind c1) (FiveOfKind c2) = ordHands (zip c1 c2)
  compare (FourOfKind c1) (FourOfKind c2) = ordHands (zip c1 c2)
  compare (FullHouse c1) (FullHouse c2) = ordHands (zip c1 c2)
  compare (ThreeOfKind c1) (ThreeOfKind c2) = ordHands (zip c1 c2)
  compare (TwoPair c1) (TwoPair c2) = ordHands (zip c1 c2)
  compare (OnePair c1) (OnePair c2) = ordHands (zip c1 c2)
  compare (HighCard c1) (HighCard c2) = ordHands (zip c1 c2)
  compare a b = compare (relativeRank a) (relativeRank b) where
    relativeRank (FiveOfKind _) = 8
    relativeRank (FourOfKind _) = 7
    relativeRank (FullHouse _) = 6
    relativeRank (ThreeOfKind _) = 5
    relativeRank (TwoPair _) = 4
    relativeRank (OnePair _) = 3
    relativeRank (HighCard _) = 2
    relativeRank InvalidHand = -1

ordHands :: Ord a => [(a, a)] -> Ordering
ordHands ((h1,h2):t) | h1 == h2 = ordHands t
ordHands ((h1,h2):_) | h1 > h2 = GT
ordHands ((h1,h2):_) | h1 < h2 = LT
ordHands _ = EQ

cardValue :: Char -> Int
cardValue c =
  case c of
    '2' -> 0
    '3' -> 1
    '4' -> 2
    '5' -> 3
    '6' -> 4
    '7' -> 5
    '8' -> 6
    '9' -> 7
    'T' -> 8
    'J' -> 9
    'Q' -> 10
    'K' -> 11
    'A' -> 12
    _ -> -1

cardValueWithJoker :: Char -> Int
cardValueWithJoker c =
    case c of
    'J' -> 0
    '2' -> 1
    '3' -> 2
    '4' -> 3
    '5' -> 4
    '6' -> 5
    '7' -> 6
    '8' -> 7
    '9' -> 8
    'T' -> 9
    'Q' -> 10
    'K' -> 11
    'A' -> 12
    _ -> -1

frequencies :: [Int] -> [(Int, Int)]
frequencies xs = Data.Map.toList (Data.Map.fromListWith (+) [(x, 1) | x <- xs])

listToHand :: [Int] -> Hand [Int]
listToHand l =
  case frequencies l of
    fs | any (\(_, f) -> f == 5) fs -> FiveOfKind l
    fs | any (\(_, f) -> f == 4) fs -> FourOfKind l
    fs | any (\(_, f) -> f == 3) fs && any (\(_, f) -> f == 2) fs -> FullHouse l
    fs | any (\(_, f) -> f == 3) fs && all (\(_, f) -> f /= 2) fs -> ThreeOfKind l
    fs | (length . filter (\(_, f) -> f == 2) $ fs) == 2 -> TwoPair l
    fs | (length . filter (\(_, f) -> f == 2) $ fs) == 1 -> OnePair l
    _ -> HighCard l

listToHandWithJoker :: [Int] -> Hand [Int]
listToHandWithJoker l = do
  let jokers = length . filter (== 0) $ l
  case frequencies l of
    fs | any (\(c, f) -> f + jokers == 5 && c /= 0) fs -> FiveOfKind l
    fs | any (\(c, f) -> f == 5 && c == 0) fs -> FiveOfKind l

    fs | any (\(c, f) -> f + jokers == 4 && c /= 0) fs -> FourOfKind l
    fs | any (\(c, f) -> f == 4 && c == 0) fs -> FourOfKind l

    fs | any (\(_, f) -> f == 3) fs && any (\(_, f) -> f == 2) fs -> FullHouse l
    fs | (length . filter (\(_, f) -> f == 2) $ fs) == 2 && jokers == 1 -> FullHouse l

    fs | any (\(_, f) -> f == 3) fs && all (\(_, f) -> f /= 2) fs -> ThreeOfKind l
    fs | (length . filter (\(_, f) -> f == 2) $ fs) == 1 && jokers == 1 -> ThreeOfKind l
    _ | jokers == 2 -> ThreeOfKind l

    fs | (length . filter (\(_, f) -> f + jokers == 2) $ fs) == 2 -> TwoPair l

    fs | (length . filter (\(_, f) -> f == 2) $ fs) == 1 -> OnePair l
    _ | jokers == 1 -> OnePair l

    _ -> HighCard l

parseHand :: (Char -> Int) -> ([Int] -> Hand [Int]) -> [String] -> (Hand [Int], Int)
parseHand p f (h:(b:_)) = (f . map p $ h, read b)
parseHand _ _ _ = (InvalidHand, 0)

parseLine :: (Char -> Int) -> ([Int] -> Hand [Int]) -> String -> (Hand [Int], Int)
parseLine p f = parseHand p f . words

parseInput :: (Char -> Int) -> ([Int] -> Hand [Int]) -> [String] -> [(Hand [Int], Int)]
parseInput p f = map (parseLine p f)

sortHands :: [(Hand [Int], Int)] -> [(Hand [Int], Int)]
sortHands = sort

totalWinnings :: (Char -> Int) -> ([Int] -> Hand [Int]) -> [String] -> Int
totalWinnings p f ls = sum . zipWith (*) (map snd . sortHands . parseInput p f $ ls) $ [1..]

main :: IO ()
main = do
  ls <- readLines "day07\\input.txt"
  print ("First solution: " ++ show (totalWinnings cardValue listToHand ls))
  print ("Second solution: " ++ show (totalWinnings cardValueWithJoker listToHandWithJoker ls))
