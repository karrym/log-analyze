module Parse where

import Control.Monad
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data

type Parse = StateT String Maybe

item :: Parse Char
item = do
  s <- get
  case s of
    [] -> empty
    (x:s') -> put s' >> return x

ident :: Char -> Parse String
ident c = do
  x <- item
  if c == x then modify (x:) >> return [] else (x:) <$> ident c

char :: Char -> Parse ()
char c = do
  x <- item
  if c == x then return () else empty

stringLit :: Parse String
stringLit = char '\"' *> ident '\"' <* char '\"'

monthMap :: M.Map String Int
monthMap = M.fromList
           [ ("Jan", 1), ("Feb", 2), ("Mar", 3), ("Apr", 4)
           , ("May", 5), ("Jun", 6), ("Jul", 7), ("Aug", 8)
           , ("Sep", 9), ("Oct", 10), ("Nov", 11), ("Dec", 12)]

getMonth :: String -> Maybe Int
getMonth = flip M.lookup monthMap

parseTimeZone :: Parse TimeZone
parseTimeZone = do
  dir <- (char '+' *> pure Plus) <|> (char '-' *> pure Minus)
  hou <- read <$> (item >>= \a -> item >>= \b -> return [a,b])
  min <- read <$> (item >>= \a -> item >>= \b -> return [a,b])
  return $ TimeZone dir hou min

parseDate :: Parse Date
parseDate = do
  day <- read <$> ident '/'
  char '/'
  month <- lift . getMonth =<< ident '/'
  char '/'
  year <- read <$> ident ':'
  char ':'
  hour <- read <$> ident ':'
  char ':'
  minute <- read <$> ident ':'
  char ':'
  second <- read <$> ident ' '
  char ' '
  zone <- parseTimeZone
  return $ Date day month year hour minute second zone

parseLog :: Parse Log
parseLog = do
  host <- Host <$> ident ' '
  char ' '
  client <- Client <$> ident ' '
  char ' '
  user <- User <$> ident ' '
  char ' '
  char '['
  date <- parseDate
  char ']'
  char ' '
  request <- Request <$> stringLit
  char ' '
  lastStatus <- Status . read <$> ident ' '
  char ' '
  size <- Size . read <$> ((char '-' *> return "0") <|> ident ' ')
  char ' '
  refer <- Refer <$> stringLit
  char ' '
  userAgent <- UserAgent <$> stringLit
  return $ Log host client user date request lastStatus size refer userAgent
