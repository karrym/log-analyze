module Main where

import Parse
import Data
import Analyze
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.Map as M
import System.Environment
import Data.List

printDateMap :: DateMap -> IO ()
printDateMap m = do
  putStrLn "Hour\t | Access"
  putStrLn "------------------------------"
  IM.toAscList m `forM_` \(h, n) -> putStrLn $ show h ++ "\t | " ++ show n

printHostMap :: HostMap -> IO ()
printHostMap m =
  forM_ xs $ \(Host h, n) -> putStrLn $ h ++ " : " ++ show n
  where xs = sortBy (\a b -> compare (snd a) (snd b)) $ M.toList m

printResult :: AnalyzeData -> IO ()
printResult (AnalyzeData dm hm) = do
  printDateMap dm
  putStrLn "\n\n"
  printHostMap hm

type Period = (UTCDate, UTCDate)

getPeriod :: [String] -> Either (Maybe String) Period
getPeriod xs =
  if length xs < 2
  then Left (Just "not enough argument of -d")
  else if length xs > 2
       then Left (Just "too many argument of -d")
       else case evalStateT parseDate (xs !! 0) of
              Nothing -> Left (Just "parse error at first argument of -d")
              Just d1 -> case evalStateT parseDate (xs !! 1) of
                           Nothing -> Left (Just "parse error at second argument of -d")
                           Just d2 -> Right (adjustDate d1, adjustDate d2)

argParse :: [String] -> [FilePath]
  -> ([FilePath], Either (Maybe String) Period)
argParse [] ps = (ps, Left Nothing)
argParse (x:xs) ps =
  if x == "-d"
  then (ps, getPeriod xs)
  else argParse xs (x:ps)

main :: IO ()
main = do
  as <- getArgs
  let (ps, opt) = argParse as []
  case opt of
          (Left (Just err)) -> putStrLn err
          (Left Nothing) -> analyze ps (const True) >>= printResult
          (Right (b, e)) -> analyze ps (\d -> b <= date d && date d <= e)
                              >>= printResult
