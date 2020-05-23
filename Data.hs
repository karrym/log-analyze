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
          deriving (Eq,Ord,Show)

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
  date :: Date,
  request :: Request,
  lastStatus :: Status,
  size :: Size,
  refer :: Refer,
  userAgent :: UserAgent }
         deriving (Eq,Ord,Show)
