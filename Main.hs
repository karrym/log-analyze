module Main where

import Control.Monad.Cont
import Parse
import Data
import System.IO
import Control.Monad.State

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

main :: IO ()
main = return ()
