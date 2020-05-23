
module Analyze where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List
import Data.Foldable
import Data.Maybe
import Control.Monad.Cont
import Parse
import Data
import System.IO
import Control.Monad.State

type DateMap = IM.IntMap Int

type HostMap = M.Map Host Integer

type AnalyzeData = (DateMap, HostMap)

{-
TODO:
日付ごとに平均
-}
accumByDate :: UTCDate -> DateMap -> DateMap
accumByDate d m = IM.insert h (IM.findWithDefault 0 h m + 1) m
  where h = utcHour d

accumByHost :: Host -> HostMap -> HostMap
accumByHost h m = M.insert h (M.findWithDefault 0 h m + 1) m


analyzeLogs :: (Log -> Bool) -> [Log] -> (DateMap, HostMap)
analyzeLogs p = foldl' (flip (stepAnalyze p)) (IM.empty, M.empty)

stepAnalyze :: (Log -> Bool) -> Log -> AnalyzeData -> AnalyzeData
stepAnalyze p l (dm, hm) =
  if p l
  then (accumByDate (date l) dm, accumByHost (host l) hm)
  else (dm, hm)

untilM :: Monad m => m Bool -> m a -> m [a]
untilM mp md = mp >>= \p ->
  if p then return [] else (:) <$> md <*> untilM mp md

getLogs :: FilePath -> IO [Log]
getLogs path = flip runContT return $ do
  h <- ContT $ withFile path ReadMode
  ls <- liftIO . untilM (hIsEOF h) . fmap (evalStateT parseLog) $ hGetLine h
  return $ catMaybes ls

analyze :: [FilePath] -> (Log -> Bool) -> IO AnalyzeData
analyze ps f = do
  ls <- foldlM (\ls p -> (++ ls) <$> getLogs p) [] ps
  return $ analyzeLogs f ls
