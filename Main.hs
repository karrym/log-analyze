module Main where

import Control.Monad.Cont
import Parse
import Data
import Analyze
import System.IO
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.Map as M
import System.Environment
import Data.List
import Data.Foldable

getLogs :: FilePath -> IO [Log]
getLogs path = flip runContT return $ do
  h <- ContT $ withFile path ReadMode
  liftIO $ getLogsFromHandle h
  where
    getLogsFromHandle h = do
      b <- hIsEOF h
      if b
        then return []
        else do
          s <- hGetLine h
          case evalStateT parseLog s of
            Nothing -> getLogsFromHandle h
            Just log -> (log :) <$> getLogsFromHandle h

printDateMap :: DateMap -> IO ()
printDateMap m = do
  putStrLn "Hour\t | Access"
  putStrLn "------------------------------"
  IM.toAscList m `forM_` \(h, n) -> putStrLn $ show h ++ "\t | " ++ show n

printHostMap :: HostMap -> IO ()
printHostMap m =
  forM_ xs $ \(Host h, n) -> putStrLn $ h ++ " : " ++ show n
  where xs = sortBy (\a b -> compare (snd a) (snd b)) $ M.toList m

getPeriod :: [String] -> Either (Maybe String) (Date, Date)
getPeriod xs =
  if length xs < 2
  then Left (Just "not enough argument of -d")
  else if length xs > 2
       then Left (Just "too many argument of -d")
       else case evalStateT parseDate (xs !! 0) of
              Nothing -> Left (Just "parse error at first argument of -d")
              Just d1 -> case evalStateT parseDate (xs !! 1) of
                           Nothing -> Left (Just "parse error at second argument of -d")
                           Just d2 -> Right (d1, d2)

argParse :: [String] -> [FilePath] -> ([FilePath], Either (Maybe String) (Date, Date))
argParse [] ps = (ps, Left Nothing)
argParse (x:xs) ps =
  if x == "-d"
  then (ps, getPeriod xs)
  else argParse xs (x:ps)

main :: IO ()
main = do
  ps <- getArgs
  ls <- foldlM (\ls p -> (++ ls) <$> getLogs p) [] ps
  let (dm, hm) = analyzeLogs ls
  printDateMap dm
  putStr "\n\n"
  printHostMap hm
