{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data where

newtype Host = Host String
             deriving (Eq,Ord,Show)

newtype Client = Client String
               deriving (Eq,Ord,Show)

newtype User = User String
             deriving (Eq,Ord,Show)

data Direction = Plus | Minus
               deriving (Eq, Ord, Show)

data TimeZone = TimeZone {
  dir :: Direction,
  hourZone :: Int,
  minZone :: Int }
  deriving (Eq, Ord, Show)

data Date = Date {
  day :: Int,
  month :: Int,
  year :: Int,
  hour :: Int,
  minute :: Int,
  second :: Int,
  zone :: TimeZone}
          deriving (Eq,Show)

data UTCDate = UTCDate {
  utcDay :: Int,
  utcMonth :: Int,
  utcYear :: Int,
  utcHour :: Int,
  utcMinute :: Int,
  utcSecond :: Int}
          deriving (Eq,Ord,Show)

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0) && (not (y `mod` 100 == 0) || (y `mod` 400 == 0))

daysOfMonth :: Int -> [Int]
daysOfMonth y =
  if isLeap y
  then [31,29,31,30,31,30,31,31,30,31,30,31]
  else [31,28,31,30,31,30,31,31,30,31,30,31]

addYear :: Int -> Date -> Date
addYear y d = d {year = year d + y}

addMonth :: Int -> Date -> Date
addMonth m d =
  let m' = month d + m
  in if m' > 12
     then addMonth (m' - 12) $ addYear 1 d {month = 1}
     else if m' < 1
             then addMonth m' $ addYear (-1) d {month = 12}
             else d {month = m'}

addDay :: Int -> Date -> Date
addDay d date =
  let d' = day date + d
      m = month date
      days = daysOfMonth (year date) !! m
  in if d' > days
        then addDay (d' - days) $ addMonth 1 date {day = 1}
        else if d' < 1
                then let date' = addMonth (-1) date
                     in addDay d' date'
                          {month = daysOfMonth (year date') !! month date'}
                else date {day = d'}

addHour :: Int -> Date -> Date
addHour h d =
  let h' = hour d + h
  in if h' > 23
        then addHour (h' - 24) $ addDay 1 d {hour = 0}
        else if h' < 0
                then addHour h' $ addDay (-1) d {hour = 23}
                else d {hour = h'}

addMin :: Int -> Date -> Date
addMin m d =
  let m' = minute d + m
  in if m' > 59
        then addMin (m' - 60) $ addHour 1 d {minute = 0}
        else if m' < 0
                then addMin m' $ addHour (-1) d {minute = 59}
                else d {minute = m'}

addSec :: Int -> Date -> Date
addSec s d =
  let s' = second d + s
  in if s' > 59
        then addSec (s' - 60) $ addMin 1 d {second = 0}
        else if s' < 0
                then addSec s' $ addMin (-1) d {second = 59}
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
                deriving (Eq,Ord,Show)

newtype Status = Status Int
               deriving (Eq,Ord,Show)

newtype Size = Size Int
             deriving (Eq,Ord,Show)

newtype Refer = Refer String
              deriving (Eq,Ord,Show)

newtype UserAgent = UserAgent String
                  deriving (Eq,Ord,Show)

data Log = Log {
  host :: Host,
  client :: Client,
  user :: User,
  date :: UTCDate,
  request :: Request,
  lastStatus :: Status,
  size :: Size,
  refer :: Refer,
  userAgent :: UserAgent }
         deriving (Eq,Ord,Show)
