{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data where

import qualified Data.Map as M
import Data.Tuple
import Data.Maybe

import GHC.Stack (HasCallStack)

newtype Host = Host String
             deriving (Eq,Ord)

instance Show Host where
  show (Host s) = s

newtype Client = Client String
               deriving (Eq,Ord)

instance Show Client where
  show (Client s) = s

newtype User = User String
             deriving (Eq,Ord)

instance Show User where
  show (User s) = s

data Direction = Plus | Minus
               deriving (Eq, Ord, Enum)

instance Show Direction where
  show Plus = "+"
  show Minus = "-"

data TimeZone = TimeZone {
  dir :: Direction,
  hourZone :: Int,
  minZone :: Int }
  deriving (Eq, Ord)

instance Show TimeZone where
  show (TimeZone d h m) = show d ++ formatInt 2 h ++ formatInt 2 m

data Date = Date {
  day :: Int,
  month :: Int,
  year :: Int,
  hour :: Int,
  minute :: Int,
  second :: Int,
  zone :: TimeZone}
          deriving (Eq)

instance Show Date where
  show d = formatInt 2 (day d)
           ++ "/" ++ fromJust (getNotation $ month d)
           ++ "/" ++ formatInt 4 (year d)
           ++ ":" ++ formatInt 2 (hour d)
           ++ ":" ++ formatInt 2 (minute d)
           ++ ":" ++ formatInt 2 (second d)
           ++ " " ++ show (zone d)

data UTCDate = UTCDate {
  utcYear :: Int,
  utcMonth :: Int,
  utcDay :: Int,
  utcHour :: Int,
  utcMinute :: Int,
  utcSecond :: Int}
          deriving (Eq,Ord)

utc2Zone :: UTCDate -> Date
utc2Zone d = Date (utcDay d) (utcMonth d) (utcYear d)
                  (utcHour d) (utcMinute d) (utcSecond d) utcZone

instance Show UTCDate where
  show d = show $ utc2Zone d

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0) && (not (y `mod` 100 == 0) || (y `mod` 400 == 0))

daysOfMonth :: HasCallStack => Int -> Int -> Int
daysOfMonth y m =
  if isLeap y
  then case [31,29,31,30,31,30,31,31,30,31,30,31] !? (m - 1) of
         Nothing -> error $ "index out of bounds in daysOfMonth: m = " ++ show m
         Just d -> d
  else case [31,28,31,30,31,30,31,31,30,31,30,31] !? (m - 1) of
         Nothing -> error $ "index out of bounds in daysOfMonth: m = " ++ show m
         Just d -> d

addYear :: Int -> Date -> Date
addYear y d = d {year = year d + y}

addMonth :: Int -> Date -> Date
addMonth m d =
  let m' = month d + m
  in if m' > 12
     then addMonth (m' - 13) $ addYear 1 d {month = 1}
     else if m' < 1
             then addMonth m' . addYear (-1) $ d {month = 12}
             else d {month = m'}

addDay :: Int -> Date -> Date
addDay d date =
  let d' = day date + d
      m = month date
      days = daysOfMonth (year date) m
  in if d' > days
        then addDay (d' - days - 1) $ addMonth 1 date {day = 1}
        else if d' < 1
                then let date' = addMonth (-1) date
                     in addDay d' $ date'
                          {day = daysOfMonth (year date') $ month date'}
                else date {day = d'}

addHour :: Int -> Date -> Date
addHour h d =
  let h' = hour d + h
  in if h' > 23
        then addHour (h' - 24) $ addDay 1 d {hour = 0}
        else if h' < 0
                then addHour h' . addDay (-1) $ d {hour = 23}
                else d {hour = h'}

addMin :: Int -> Date -> Date
addMin m d =
  let m' = minute d + m
  in if m' > 59
        then addMin (m' - 60) $ addHour 1 d {minute = 0}
        else if m' < 0
                then addMin m' . addHour (-1) $ d {minute = 59}
                else d {minute = m'}

addSec :: Int -> Date -> Date
addSec s d =
  let s' = second d + s
  in if s' > 59
        then addSec (s' - 60) $ addMin 1 d {second = 0}
        else if s' < 0
                then addSec s' . addMin (-1) $ d {second = 59}
                else d {second = s'}

utcZone :: TimeZone
utcZone = TimeZone Plus 0 0

adjustDate :: Date -> UTCDate
adjustDate d =
  let z = zone d
      diff = minZone z + 60 * hourZone z
      utcDate = case dir z of
                  Plus -> addMin diff d {zone = utcZone}
                  Minus -> addMin (-diff) d {zone = utcZone}
      d2utc d = UTCDate (day d) (month d) (year d) (hour d) (minute d) (second d)
  in d2utc utcDate

instance Ord Date where
  d1 <= d2 = adjustDate d1 <= adjustDate d2

newtype Request = Request String
                deriving (Eq,Ord)

instance Show Request where
  show (Request s) = s

newtype Status = Status Int
               deriving (Eq,Ord)

instance Show Status where
  show (Status i) = show i

newtype Size = Size Int
             deriving (Eq,Ord)

instance Show Size where
  show (Size i) = if i == 0 then "-" else show i

newtype Refer = Refer String
              deriving (Eq,Ord)

instance Show Refer where
  show (Refer s) = s

newtype UserAgent = UserAgent String
                  deriving (Eq,Ord)

instance Show UserAgent where
  show (UserAgent s) = s

data Log = Log {
  host :: Host,
  client :: Client,
  user :: User,
  date :: Date,
  request :: Request,
  lastStatus :: Status,
  size :: Size,
  refer :: Refer,
  userAgent :: UserAgent }
         deriving (Eq,Ord)

instance Show Log where
  show l = show (host l)
           ++ " " ++ show (client l)
           ++ " " ++ show (user l)
           ++ " [" ++ show (date l) ++ "]"
           ++ " \"" ++ show (request l) ++ "\""
           ++ " " ++ show (lastStatus l)
           ++ " " ++ show (size l)
           ++ " \"" ++ show (refer l) ++ "\""
           ++ " \"" ++ show (userAgent l) ++ "\""

type Digit = Int

formatInt :: Digit -> Int -> String
formatInt 0 _ = ""
formatInt n i = formatInt (n-1) (i `div` 10) ++ show (i `mod` 10)

monthList = [ ("Jan", 1), ("Feb", 2), ("Mar", 3), ("Apr", 4)
           , ("May", 5), ("Jun", 6), ("Jul", 7), ("Aug", 8)
           , ("Sep", 9), ("Oct", 10), ("Nov", 11), ("Dec", 12)]

monthMap :: M.Map String Int
monthMap = M.fromList monthList

monthMapInv :: M.Map Int String
monthMapInv = M.fromList $ map swap monthList

getMonth :: String -> Maybe Int
getMonth = flip M.lookup monthMap

getNotation :: Int -> Maybe String
getNotation = flip M.lookup monthMapInv


