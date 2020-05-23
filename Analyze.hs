
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

type DateMap = IM.IntMap Integer

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

emptyData :: AnalyzeData
emptyData = (IM.empty, M.empty)

untilM :: Monad m => m Bool -> a -> (a -> m a) -> m a
untilM mp a md = mp >>= \p ->
  if p then return a else md a >>= flip (untilM mp) md

analyzeFile :: (Log -> Bool) -> FilePath -> AnalyzeData -> IO AnalyzeData
analyzeFile p path ad = flip runContT return $ do
  h <- ContT $ withFile path ReadMode
  liftIO . untilM (hIsEOF h) ad $ stepLog h p

stepLog :: Handle -> (Log -> Bool) -> AnalyzeData -> IO AnalyzeData
stepLog h p ad =
  maybe ad (flip (stepAnalyze p) ad) . evalStateT parseLog <$> hGetLine h

analyze :: [FilePath] -> (Log -> Bool) -> IO AnalyzeData
analyze ps f = foldlM (flip (analyzeFile f)) emptyData ps
