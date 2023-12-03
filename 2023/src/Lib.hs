module Lib (readLines, mapIndex) where

import Paths_adventofcode2023 (getDataFileName)

readLines :: FilePath -> IO [String]
readLines fileName = do
  path <- getDataFileName fileName
  contents <- readFile path
  pure $ lines contents

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..]
