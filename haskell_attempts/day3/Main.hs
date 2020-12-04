module Main where

import Data.Text(
  split,
  pack,
  unpack)
import Data.List(filter)

main :: IO ()
main = do
  xs <- getListOfStrings
  putStrLn (show $ countHashes (3,1) xs)
  let dirs = [(1,1),(3,1),(5,1),(7,1),(1,2)]
  let result = foldl (*) 1 [countHashes d xs | d <- dirs]
  putStrLn (show result)


f :: IO String
f = readFile "../../input3.txt"

separateLines :: String -> [String]
separateLines x = fmap unpack (split (=='\n') (pack x))

getListOfStrings :: IO [String]
getListOfStrings = do
  file <- f
  let listOfStrings = separateLines file
  return (filter (/="") listOfStrings)

countHashes :: (Int,Int) ->[String]  -> Int
countHashes dir list =
  foldl (hasHash dir) 0 (zip list [0..])

hasHash :: (Int, Int) -> Int -> (String, Int) -> Int
hasHash (right, down) acc (line, i)
  | (mod i down) == 0 && line !! position == '#' = acc+1
  | otherwise = acc
  where position = mod (right * (div i down) ) (length line)
