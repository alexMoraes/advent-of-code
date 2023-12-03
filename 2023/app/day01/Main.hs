import Paths_adventofcode2023 (getDataFileName)
import qualified Data.Text as T
import Data.List (foldl')

readLines :: FilePath -> IO [String]
readLines fileName = do
  path <- getDataFileName fileName
  contents <- readFile path
  pure $ lines contents

replaceSpelledDigits :: String -> String
replaceSpelledDigits = foldl' appendAndReplace ""

appendAndReplace ::  String -> Char -> String
appendAndReplace s c = T.unpack
  . T.replace (T.pack "one") (T.pack "1")
  . T.replace (T.pack "two") (T.pack "2")
  . T.replace (T.pack "three") (T.pack "3")
  . T.replace (T.pack "four") (T.pack "4")
  . T.replace (T.pack "five") (T.pack "5")
  . T.replace (T.pack "six") (T.pack "6")
  . T.replace (T.pack "seven") (T.pack "7")
  . T.replace (T.pack "eight") (T.pack "8")
  . T.replace (T.pack "nine") (T.pack "9")
  . T.pack
  $ s ++ [c]

filterDigits :: String -> String
filterDigits line = [ c | c <- line, c `elem` "0123456789" ]

addDigits :: [Char] -> Int
addDigits [] = 0
addDigits [c] = read [c, c]
addDigits (c:cs) = read [c,last cs]

getSimpleCalibration :: String -> Int
getSimpleCalibration = addDigits . filterDigits

getCalibration :: String -> Int
getCalibration = addDigits . filterDigits . replaceSpelledDigits

main :: IO ()
main = do
  ls <- readLines "day01\\input.txt"
  let simpleCalibrations = map getSimpleCalibration ls
  print ("First solution: " ++ show (sum simpleCalibrations))
  let calibrations = map getCalibration ls
  print ("Second solution: " ++ show (sum calibrations))