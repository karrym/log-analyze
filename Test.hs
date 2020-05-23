module Main where

import System.IO
import System.Random
import Control.Monad.Cont
import System.Environment

import Data

import GHC.Stack (HasCallStack)

testAgent :: UserAgent
testAgent = UserAgent "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"

testRequests :: [Request]
testRequests = map Request [ "GET / HTTP/1.1"
                           , "GET /style.css HTTP/1.1"
                           , "GET /img/title.png HTTP/1.1"]

testHosts :: [Host]
testHosts = map Host [ "10.2.3.4"
                     , "50.6.7.8"
                     , "90.10.20.40"]

testUser :: User
testUser = User "-"

testClient :: Client
testClient = Client "-"

testStatus :: Status
testStatus = Status 200

testSize :: Size
testSize = Size 300

testRefer :: Refer
testRefer = Refer "test://computer.domain.test/"

genTimeZone :: HasCallStack => IO TimeZone
genTimeZone = do
  m <- ([0,30] !?) <$> randomRIO (0, 1)
  case m of
    Nothing -> error "index out of bounds in genTimeZone"
    Just i -> TimeZone <$> (toEnum <$> randomRIO (0, 1))
                       <*> randomRIO (0,10)
                       <*> pure i


genDate :: HasCallStack => IO Date
genDate = do
  year <- (+ 2000) <$> randomRIO (-10,10)
  month <- randomRIO (1, 12)
  day <- randomRIO (1, daysOfMonth year month)
  hour <- randomRIO (0, 23)
  minute <- randomRIO (0, 59)
  second <- randomRIO (0, 59)
  zone <- genTimeZone
  return $ Date day month year hour minute second zone

genLog :: HasCallStack => IO Log
genLog = do
  m <- (testRequests !?) <$> randomRIO (0, length testRequests - 1)
  case m of
    Nothing -> error "index out of bounds in genLog at testRequests"
    Just r -> do
      m' <- (testHosts !?) <$> randomRIO (0, length testHosts - 1)
      case m' of
        Nothing -> error "index out of bounds in genLog at testHosts"
        Just h -> do
          date <- genDate
          return $ Log h testClient testUser (utc2Zone $ adjustDate date) r testStatus testSize testRefer testAgent

genTestFile :: FilePath -> Int -> IO ()
genTestFile path size = withFile path WriteMode (go size)
  where go 0 _ = return ()
        go n h = genLog >>= hPrint h >> go (n-1) h

main :: IO ()
main = do
  [path, lines] <- getArgs
  genTestFile path (read lines)
