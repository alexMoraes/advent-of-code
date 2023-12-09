import Lib (readLines)

choose :: Int -> Int -> Int
_ `choose` 0 = 1
0 `choose` _ = 0
n `choose` k = (n-1) `choose` (k-1) * n `div` k

next :: [Int] -> Int
next l =
  sum . zipWith (*) (map (length l `choose`) [1..]) . zipWith (*) (cycle [1,-1]) $ l

totalNext :: [String] -> Int
totalNext = sum . map ((next . reverse . map read) . words)

totalPrevious :: [String] -> Int
totalPrevious = sum . map ((next . map read) . words)

main :: IO ()
main = do
  ls <- readLines "day09\\input.txt"
  print ("First solution: " ++ show (totalNext ls))
  print ("Second solution: " ++ show (totalPrevious ls))
