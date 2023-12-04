import qualified Data.Text as T (splitOn, pack, strip, unpack, replace)
import Lib (readLines)
import Data.Foldable ( Foldable(foldl') )

parseLine :: String -> ([Int], [Int])
parseLine l = do
  let splitParts [] = ("", "")
      splitParts [_] = ("", "")
      splitParts [h1, h2] = (T.unpack . T.strip $ h1, T.unpack . T.strip $ h2)
      splitParts (h1:(h2:_)) = (T.unpack . T.strip $ h1, T.unpack . T.strip $ h2)
  let (_, input) = splitParts . T.splitOn (T.pack ":") . T.pack $ l
  let (winningNumbers, myNumbers) = splitParts . T.splitOn (T.pack "|") . T.pack $ input
  (map (read . T.unpack) . T.splitOn (T.pack " ") . T.replace (T.pack "  ") (T.pack " ") . T.pack $ winningNumbers, map (read. T.unpack) . T.splitOn (T.pack " ") . T.replace (T.pack "  ") (T.pack " ") . T.pack $ myNumbers)

cardPoints :: ([Int], [Int]) -> Int
cardPoints (winningNumbers, myNumbers) = do
  let calcPoints n 0 | n `elem` winningNumbers = 1
      calcPoints n p | n `elem` winningNumbers = p*2
      calcPoints _ p = p
  foldr calcPoints 0 myNumbers

matchingNumbers :: ([Int], [Int]) -> Int
matchingNumbers (winningNumbers, myNumbers) = do
  let calcPoints n p | n `elem` winningNumbers = p + 1
      calcPoints _ p = p
  foldr calcPoints 0 myNumbers

totalPoints :: [String] -> Int
totalPoints = foldr (\l p -> p + (cardPoints . parseLine $ l)) 0

countCopies :: (Int, [Int]) -> Int -> (Int, [Int])
countCopies (t, []) _ = (t, [])
countCopies (t, m:ms) p = (t+m, (map (m +) . take p $ ms) ++ drop p ms)

totalCards :: [String] -> Int
totalCards = fst . foldl' countCopies (0, repeat 1) . map (matchingNumbers . parseLine)

main :: IO ()
main = do
  ls <- readLines "day04\\input.txt"
  print ("First solution: " ++ show (totalPoints ls))
  print ("Second solution: " ++ show (totalCards ls))
