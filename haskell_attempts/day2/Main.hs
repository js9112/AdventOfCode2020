module Main where
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = do
  file <- f
  let x = parse parseFile "" file
  case x of
    Left err -> print err
    Right ls -> do
      print (myCount isValid ls)
      print (myCount isValid2 ls)

f :: IO String
f = readFile "../../input2.txt"

type Parser = Parsec String ()

data MyLine = MyLine Int Int Char String deriving (Show)

parseLine :: Parser MyLine
parseLine = do
  low <- fmap read (many1 digit)
  char '-'
  high <- fmap read (many1 digit)
  char ' '
  c <- alphaNum
  string ": "
  password <- many1 alphaNum
  return (MyLine low high c password)

parseFile :: Parser [MyLine]
parseFile = do
  ls <- parseLine `endBy1` newline
  eof
  return ls

isValid :: MyLine -> Bool
isValid (MyLine low high c password) =
  x>=low && x<=high
  where x = myCount (==c) password

isValid2 :: MyLine -> Bool
isValid2 (MyLine low high c password) =
    ((password !! (low-1)) == c) /= ((password !! (high-1)) == c)

myCount :: (t -> Bool) -> [t] -> Int
myCount x = length . filter (x)
