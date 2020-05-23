{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data where

newtype Host = Host String
             deriving (Show)

newtype Client = Client String
               deriving (Show)

newtype User = User String
             deriving (Show)

data Date = Date {
  day :: Int,
  month :: Int,
  year :: Int,
  hour :: Int,
  minute :: Int,
  second :: Int,
  zone :: String}
          deriving (Show)

newtype Request = Request String
                deriving (Show)

newtype Status = Status Int
               deriving (Show)

newtype Size = Size Int
             deriving (Show)

newtype Refer = Refer String
              deriving (Show)

newtype UserAgent = UserAgent String
                  deriving (Show)

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
         deriving (Show)
