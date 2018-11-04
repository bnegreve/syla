module Coinche.Logs where

import Coinche.Engine.Types
import Data.Binary
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Maybe

readLogs :: IO (Game,GameStats)
readLogs = decode <$> B.readFile "games.log"

game = fst <$> readLogs

stats :: IO GameStats
stats = snd <$> readLogs

lookupDistribs :: Player -> IO [Distribution]
lookupDistribs p = do
  (GameStats m) <- stats
  pure $ fromJust $ M.lookup p m

  
