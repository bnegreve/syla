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

type Ai = Game -> Player -> Atout -> [Card] -> IO Card

-- AI: plays any card that is valid
dumbAi :: Ai
dumbAi game player trump legalcards = do
  head <$> shuffleM legalcards

basicAi :: GenericAiOpts -> Game -> Player -> Atout -> [Card] -> IO Card
basicAi (GAO _ _ nsim) game player trump legalcards 
  = basicAi' trump nsim game player legalcards

-- AI: runs n rollouts for each move and pick whatever move lead to the
-- best average score. 
-- Cheats because it knows the full game state (all players hands)
basicAi' :: Atout -> Int -> Game -> Player -> [Card] -> IO Card
basicAi' atout n game player legalcards = do 
  cardscores <- simul atout n game player legalcards
  pure $ bestmove cardscores

iimcAi :: GenericAiOpts -> Ai
iimcAi (GAO _ ngames nsim) game player trump legalcards = 
  iimcAi' trump ngames nsim game player legalcards

-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Warning: In this versions the games are sampled without analyzing
-- the previous turns
iimcAi' :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
iimcAi' atout ngames nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> do 
    possiblegame <- samplePossibleGame pogame player remcards
    simul atout nsim possiblegame player legalcards -- :: [[(Card, Double)]]
  -- sum the scores for each card accross the n simulated games
  let cardscores =
        (\x -> (fst $ head x, sum $ snd <$> x))
        <$> transpose allscores :: [(Card,Double)] in 
    pure $ bestmove cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player

iimcAiSmart :: GenericAiOpts -> Ai
iimcAiSmart (GAO _ ngames nsim) game player trump legalcards = 
  iimcAiSmart' trump ngames nsim game player legalcards

-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Derived from algo2 in:
--  Furtak & Buro : Recursive Monte Carlo Search for Imperfect Information Games
iimcAiSmart' :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
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
  let cardscores =
        (\x -> (fst $ head x, sum $ snd <$> x))
        <$> transpose allscores :: [(Card,Double)] in do
--    putStrLn ""
    pure $ bestmove cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player



