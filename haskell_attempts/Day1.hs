module Day1 where

import Data.Text(
  split,
  pack,
  unpack)
import Data.List(filter)

f :: IO String
f = readFile "../input1a.txt"

separateLines :: String -> [String]
separateLines x = fmap unpack (split (=='\n') (pack x))

getListOfNumbers :: IO [Int]
getListOfNumbers = do
  file <- f
  let listOfStrings = separateLines file
  return (map read (filter (/="") listOfStrings))

findSum3 :: Int -> [Int] -> Maybe [Int]
findSum3 sumTo (x:ys)
  | findSum (sumTo-x) ys == Nothing = findSum3 sumTo ys
  | otherwise = fmap (x :) (findSum (sumTo-x) ys)
findSum3 _ _ = Nothing

findSum :: Int -> [Int] -> Maybe [Int]
findSum sumTo (x:ys)
  | findMatch (sumTo-x) ys == Nothing = findSum sumTo ys
  | otherwise = Just [x, maybe 0 id (findMatch (sumTo-x) ys)]
findSum _ _ = Nothing

findSum' :: Int -> [Int] -> Maybe (Int, Int)
findSum' sumTo (x:ys) =
  case findMatch (sumTo-x) ys of
    Nothing -> findSum' sumTo ys
    Just a -> Just (x, a)
findSum' _ _ = Nothing

findMatch :: Int -> [Int] -> Maybe Int
findMatch _ [] = Nothing
findMatch x (y:zs)
  | x == y = Just y
  | otherwise = findMatch x zs
