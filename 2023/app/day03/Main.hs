import Lib (readLines, mapIndex)

data PartNumber = PartNumber {
  partNumber :: Int,
  positions :: [(Int, Int)]
} deriving (Show)

isPartSymbol :: Char -> Bool
isPartSymbol s = s `notElem` ['.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

getPartsPositions :: [String] -> [(Int, Int)]
getPartsPositions = map (\(_, r, c) -> (r, c))
  . filter (\(s, _, _) -> isPartSymbol s)
  . concat
  . mapIndex (\l r -> mapIndex (\s c -> (s, r, c)) l)

accumulateParts :: (Char, Int, Int) -> (String, [(Int, Int)], [PartNumber]) -> (String, [(Int, Int)], [PartNumber])
accumulateParts (s, _, _) ("", _, parts) | s `notElem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] = ("", [], parts)
accumulateParts (s, _, _) (cur, curInd, parts) | s `notElem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] = ("", [], PartNumber (read cur) curInd : parts)
accumulateParts (s, r, c) (cur, curInd, parts) = (s : cur, (r, c) : curInd, parts)

accumulateLines :: (String, Int) -> [PartNumber] -> [PartNumber]
accumulateLines (l, r) acc = do
  let (_, _, parts) = foldLine l r
  acc ++ parts

foldLine :: String -> Int -> (String, [(Int, Int)], [PartNumber])
foldLine l r = foldr (\(s, c) acc -> accumulateParts (s, r, c) acc) ("", [], []) (zip ('.':l) [-1..])

foldInput :: [String] -> [PartNumber]
foldInput ls = foldr (\(l,r) acc -> accumulateLines (l,r) acc) [] (zip ls [0..])

isValidPart :: Foldable t => t (Int, Int) -> PartNumber -> Bool
isValidPart ps =
  any (\(r, c) ->
    (r-1, c-1) `elem` ps ||
    (r-1, c) `elem` ps ||
    (r-1, c+1) `elem` ps ||
    (r, c-1) `elem` ps ||
    (r, c) `elem` ps ||
    (r, c+1) `elem` ps ||
    (r+1, c-1) `elem` ps ||
    (r+1, c) `elem` ps ||
    (r+1, c+1) `elem` ps) . positions

getParts :: Foldable t => t (Int, Int) -> [String] -> [PartNumber]
getParts ps = filter (isValidPart ps) . foldInput

sumParts :: [String] -> Int
sumParts ls = sum . map partNumber . getParts (getPartsPositions ls) $ ls

gearRatio ::  [Int] -> Int
gearRatio [] = 0
gearRatio [_] = 0
gearRatio (h1:h2:_) = h1 * h2

getAdjacentParts :: Int -> Int -> [PartNumber] -> [PartNumber]
getAdjacentParts r c = filter (any (\p -> p `elem` [
    (r-1, c-1),
    (r-1, c),
    (r-1, c+1),
    (r, c-1),
    (r, c),
    (r, c+1),
    (r+1, c-1),
    (r+1, c),
    (r+1, c+1)
  ]) . positions)

getLineGears :: [PartNumber] -> String -> Int -> [[PartNumber]]
getLineGears ps l r = filter ((==) 2 . length) . map (\(_, c) -> getAdjacentParts r c ps) . filter (\(s, _) -> s == '*') $ zip l [0..]

getGears :: [PartNumber] -> [String] -> [[PartNumber]]
getGears ps ls = concat (zipWith (getLineGears ps) ls [0..])

sumGearRatios :: [PartNumber] -> [String] -> Int
sumGearRatios ps ls = sum (map (gearRatio . map partNumber) (getGears ps ls))

main :: IO ()
main = do
  ls <- readLines "day03\\input.txt"
  let ps = foldInput ls
  print ("First solution: " ++ show (sumParts ls))
  print ("Second solution: " ++ show (sumGearRatios ps ls))