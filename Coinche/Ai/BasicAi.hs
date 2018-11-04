module Coinche.Ai.BasicAi where

import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Engine
import Coinche.Ai.Tools
import System.Random.Shuffle
import System.Random
import Data.List
import qualified Data.Array as A
import System.Environment 
import qualified Data.Map as M


data GenericAiOpts = GAO { _gaoAiName :: String, 
                           _gaoNGames :: Int,
                           _gaoNSim :: Int }

type Ai = Game -> Player -> [Card] -> CardGameTIO [(Card, Double)]

-- AI: plays any card that is valid
dumbAi :: Ai
dumbAi game player legalcards = do
  liftIO $ zip  <$> shuffleM legalcards <*> pure (1:repeat 0)

basicAi :: GenericAiOpts -> Ai 
basicAi (GAO _ _ nsim) game player legalcards = do
  trump <- askTrump 
  liftIO $ basicAi' trump nsim game player legalcards

-- AI: runs n rollouts for each move and pick whatever move lead to the
-- best average score. 
-- Cheats because it knows the full game state (all players hands)
basicAi' :: Atout -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
basicAi' atout n game player legalcards = do 
  cardscores <- simul atout n game player legalcards
  pure $ [(ci,score/fromIntegral m) | (ci,m,score) <- cardscores]

iimcAi :: GenericAiOpts -> Ai
iimcAi (GAO _ ngames nsim) game player legalcards = do
  trump <- askTrump
  liftIO $ iimcAi' trump ngames nsim game player legalcards

-- score aggregation function: averages the average scores accross all games 
avgAvgAgg :: [(Int, Double)] -> Double
avgAvgAgg cardscores = avgAvg
  where nonnulls = [ (s / fromIntegral n) | (n, s) <- cardscores, n /= 0 ]
        n = length nonnulls
        s = sum nonnulls
        avgAvg = if n /= 0 then s / fromIntegral n else 0.0
        
-- Aggregates card scores accross different games (compute average score)
aggregateScores :: [[(Card, Int, Double)]] -> [(Card, Double)]
aggregateScores allscores =  [(card, aggscore) | (card, scores) <- M.assocs $ scoremaps,
                              let aggscore = avgAvgAgg scores ]
  where buildcardmaps :: [(Card,Int,Double)] -> M.Map Card [(Int,Double)]
        buildcardmaps cardtriples = M.fromList [ (c, [(n,s)]) | (c, n, s) <- cardtriples ]
        mergemaps :: [M.Map Card [(Int, Double)]] -> M.Map Card [(Int,Double)]
        mergemaps cardmaps = foldr1 (M.unionWith (++)) cardmaps
        scoremaps = mergemaps $ fmap buildcardmaps allscores
        
-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Warning: In this versions the games are sampled without analyzing
-- the previous turns
iimcAi' :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
iimcAi' atout ngames nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> do 
    possiblegame <- samplePossibleGame pogame player remcards
    simul atout nsim possiblegame player legalcards
  -- sum the scores for each card accross the n simulated games
  let cardscores = aggregateScores allscores
  -- print cardscores
  pure $ cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player

    
iimcAiSmart :: GenericAiOpts -> Ai
iimcAiSmart (GAO _ ngames nsim) game player legalcards = do 
  trump <- askTrump
  liftIO $ iimcAiSmart' trump ngames nsim game player legalcards

-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Derived from algo2 in:
--  Furtak & Buro : Recursive Monte Carlo Search for Imperfect Information Games
iimcAiSmart' :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
iimcAiSmart' atout ngames nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> do
    remcardsshuffled <- shuffleM remcards
    possiblegame <- samplePossibleGameSmart pogame player remcardsshuffled
    if isJust possiblegame then do
--      putStr "*"
      simul atout nsim (fromJust possiblegame) player legalcards -- :: [[(Card, Double)]]
    else do
--      putStr "."
      possiblegame <- samplePossibleGame pogame player remcards
      simul atout nsim possiblegame player legalcards -- :: [[(Card, Double)]]
  -- sum the scores for each card accross the n simulated games
  let cardscores = aggregateScores allscores
--    putStrLn ""
  pure $ cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from playe



