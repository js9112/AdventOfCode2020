module Main where
import Data.List

main :: IO ()
main = do
  seatIds <- fmap convertToIds f
  print (findMax seatIds)
  print (findMissing (sort seatIds))

f :: IO String
f = readFile "../../input5.txt"

replace :: Char -> Char
replace 'F' = '0'
replace 'L' = '0'
replace 'B' = '1'
replace 'R' = '1'
replace x = x

convertToIds :: String -> [Int]
convertToIds s = map readBase2 $ lines (map replace s)

readBase2 :: String -> Int
readBase2 [] = 0
readBase2 ('0':ys) = readBase2 ys
readBase2 ('1':ys) = (2^(length ys)) + readBase2 ys

-- more efficient version
readBase2' :: String -> Int
readBase2' = readBase2Aux . reverse
  where
    readBase2Aux [] = 0
    readBase2Aux ('1':xs) = 1 + 2*(readBase2Aux xs)
    readBase2Aux ('0':xs) = 2*(readBase2Aux xs)

findMax :: [Int] -> Int
findMax x = foldl max (-1) x

findMissing :: [Int] -> Maybe Int
findMissing [] = Nothing
findMissing (x:y:zs) = case y-x of
  1 -> findMissing (y:zs)
  2 -> Just (x+1)
  _ -> Nothing
