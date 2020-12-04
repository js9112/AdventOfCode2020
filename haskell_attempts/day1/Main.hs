module Main where

import Data.Text(
  split,
  pack,
  unpack)
import Data.List(filter)

main :: IO ()
main = do
  x <- fmap (findSum' 2 2020) getListOfNumbers
  putStrLn (show x)
  x <- fmap (findSum' 3 2020) getListOfNumbers
  putStrLn (show x)

f :: IO String
f = readFile "../../input1a.txt"

separateLines :: String -> [String]
separateLines x = fmap unpack (split (=='\n') (pack x))

getListOfNumbers :: IO [Int]
getListOfNumbers = do
  file <- f
  let listOfStrings = separateLines file
  return (map read (filter (/="") listOfStrings))

findSum' :: Int -> Int -> [Int] -> Maybe Int
findSum' 1 _ [] = Nothing
findSum' 1 sumTo (y:zs)
  | sumTo == y = Just y
  | otherwise = findSum' 1 sumTo zs
findSum' n sumTo (x:ys) = if n<0 then Nothing else
    case findSum' (n-1) (sumTo-x) ys of
        Nothing -> findSum' n sumTo ys
        Just a -> Just (x * a)
findSum' _ _ _ = Nothing
