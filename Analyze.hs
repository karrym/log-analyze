
module Analyze where

import Data
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List

type DateMap = IM.IntMap Int

type HostMap = M.Map Host Integer

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
analyzeLogs p ls = ( foldl' (\m l -> accumByDate (date l) m) IM.empty ls'
                , foldl' (\m l -> accumByHost (host l) m) M.empty ls')
  where ls' = filter p ls
