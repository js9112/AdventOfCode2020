module Main where
import Text.Parsec
import Text.Parsec.Char
import Data.Maybe
import Data.Either

main :: IO ()
main = do
  file <- f
  let x = parse parseFile "" file
  case x of
    Left err -> print err
    Right ls -> do
      print (length (catMaybes ls))
      print (myCount validatePassport (catMaybes ls))

f :: IO String
f = readFile "../../input4.txt"

type Parser = Parsec String ()

data Passport = Passport {
  byr :: String,
  iyr :: String,
  eyr :: String,
  hgt :: String,
  hcl :: String,
  ecl :: String,
  pid :: String
} deriving (Show)

parseFile :: Parser [Maybe Passport]
parseFile = do
  ls <- parsePassport `sepBy1` (string "\n")
  eof
  return ls

parsePassport :: Parser (Maybe Passport)
parsePassport = do
  ls <- parseEntry `endBy1` space
  return $ do
    byr <- lookup "byr" ls
    iyr <- lookup "iyr" ls
    eyr <- lookup "eyr" ls
    hgt <- lookup "hgt" ls
    hcl <- lookup "hcl" ls
    ecl <- lookup "ecl" ls
    pid <- lookup "pid" ls
    return (Passport byr iyr eyr hgt hcl ecl pid)

parseEntry :: Parser (String,String)
parseEntry = do
  key <- count 3 letter
  char ':'
  value <- many1 (noneOf " \n")
  return (key,value)

validatePassport :: Passport -> Bool
validatePassport p =
  (read (byr p) >=1920 && read (byr p) <=2002)
  && (read (iyr p) >=2010 && read (iyr p) <=2020)
  && (read (eyr p) >=2020 && read (eyr p) <=2030)
  && isHashFollowedBySix (hcl p)
  && isValidHeight (hgt p)
  && elem (ecl p) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  && isNineDigits (pid p)

isHashFollowedBySix :: String -> Bool
isHashFollowedBySix s = isRight $ parse parser "" s
  where parser = char '#' >> count 6 (oneOf ['a'..'f'] <|> digit) >> eof

isNineDigits :: String -> Bool
isNineDigits s = isRight $ parse parser "" s
    where parser = count 9 digit >> eof

isValidHeight :: String -> Bool
isValidHeight s = isRight $ parse parser "" s
  where parser = do
          nbr <- fmap read (many1 digit)
          units <- string "cm" <|> string "in"
          eof
          case units of
            "cm" -> if nbr>=150 && nbr<=193 then return () else fail ""
            "in" -> if nbr>=59 && nbr<=76 then return () else fail ""

myCount :: (t -> Bool) -> [t] -> Int
myCount x = length . filter (x)
