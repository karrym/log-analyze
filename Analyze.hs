{-# LANGUAGE Strict #-}

module Analyze where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List
import Data.Foldable
import Data.Maybe
import Control.Monad.Cont
import System.IO
import Control.Monad.State
import Data.IORef

import Parse
import Data

type DateMap = IM.IntMap Integer

type HostMap = M.Map Host Integer

data AnalyzeData = AnalyzeData !DateMap !HostMap

{-
TODO:
日付ごとに平均
-}
accumByDate :: UTCDate -> DateMap -> DateMap
accumByDate = IM.alter (Just . maybe 1 (+1)) . utcHour

accumByHost :: Host -> HostMap -> HostMap
accumByHost = M.alter (Just . maybe 1 (+1))

analyzeLogs :: (Log -> Bool) -> [Log] -> AnalyzeData
analyzeLogs p = foldl' (flip (stepAnalyze p)) emptyData

stepAnalyze :: (Log -> Bool) -> Log -> AnalyzeData -> AnalyzeData
stepAnalyze p l a@(AnalyzeData dm hm) =
  if p l
  then AnalyzeData (accumByDate (date l) dm) (accumByHost (host l) hm)
  else a

emptyData :: AnalyzeData
emptyData = AnalyzeData IM.empty M.empty

untilM :: Monad m => m Bool -> a -> (a -> m a) -> m a
untilM mp a md = mp >>= \p ->
  if p then return a else md a >>= flip (untilM mp) md

untilM_ :: Monad m => m Bool -> m () -> m ()
untilM_ mp md = mp >>= \p -> if p then return () else md >> untilM_ mp md

analyzeFile :: (Log -> Bool) -> FilePath -> AnalyzeData -> IO AnalyzeData
analyzeFile p path ad = flip runContT return $ do
  ref <- liftIO $ newIORef ad
  h <- ContT $ withFile path ReadMode
  liftIO . untilM_ (hIsEOF h) $ stepLog h p ref
  liftIO $ readIORef ref

stepLog :: Handle -> (Log -> Bool) -> IORef AnalyzeData -> IO ()
stepLog h p ref =
  maybe (return ()) (modifyIORef' ref . stepAnalyze p)
      . evalStateT parseLog =<< hGetLine h

analyze :: [FilePath] -> (Log -> Bool) -> IO AnalyzeData
analyze ps f = foldlM (flip (analyzeFile f)) emptyData ps
