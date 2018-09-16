module Coinche.Ai where

import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Types
import Coinche.Game
import Coinche.Mcts
import System.Random.Shuffle
import System.Random
import Coinche.Rules
import Data.List
import qualified Data.Array as A
import System.Environment 

-- simply returns the best move in a list of tuples (moves, scores)
bestmove :: [(m, Double)] -> m
bestmove movescores =
  let max = maximumBy (\(m,s) (m',s') -> s `compare` s') movescores in
    fst max

-- Runs n rollouts for each possible card and returns the average
-- score for each card.
simul :: Atout -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
simul atout n game player cards = do
  forM cards $ \c -> do
    cardscores <- forM [1..n] $ \_ -> do
      rollout player atout (jouerCarte' atout game c) 
    let cardavgscore = sum cardscores / fromIntegral n
    pure (c, cardavgscore)

-- Returns the cards that have not been seen by player
-- (cards that have not been played and are not in player's hand)
-- shuffles the output the the ai cannot learn anything from the order.
remainingCards :: Game -> Player -> IO [Card]
remainingCards g p = do
  shuffleM $ concat [mi | (pi,Hand mi) <- A.assocs $ _gPlayersHands g, pi /= p]

-- Build random hand for each player given a set of remaining cards
-- and a hand size of each player
-- Warning: remainingCards should be randomized before 
buildRandomHands :: [Card] -> [(Player,Int)] -> [(Player,Hand)]
buildRandomHands remainingCards sizes = fst $ foldr f ([],remainingCards) sizes
  where f (player,size) (hands,remCards) = let (hand, rest) = splitAt size remCards
                                  in ((player,Hand hand) :hands, rest)


-- Returns the partially observed game from player's perspective
partiallyObservedGame :: Player -> Game -> Game
partiallyObservedGame p g = 
  g { _gPlayersHands =
        _gPlayersHands g A.// [(pi, Hand []) | pi <- [P_1 .. P_4], pi /= p]  }

-- Sample a possible game that is compatible with the current player game
-- Warning: playergame must be a partially observed game (obtained with playerGame)
samplePossibleGame :: Game -> Player -> [Card] -> IO Game
samplePossibleGame g p remcards = do
  let randomhands = buildRandomHands remcards $ 
                              [(pi,phandsize) | pi <- mustPlay]
                              ++ [(pi,phandsize - 1) | pi <- havePlayed]
  pure $ g & gPlayersHands %~ (A.// randomhands)
  where phandsize = length $ view _w $ _gPlayersHands g A.! p
        mustPlay = _gJoueursRestants g \\ [p]
        havePlayed =  (snd <$> (g ^. gPliCourant . _w)) \\ [p]


-- AI: plays any card that is valid
dumbAi :: Atout -> Int -> Game -> Player -> [Card] -> IO Card
dumbAi atout n game player legalcards = do
  head <$> shuffleM legalcards

-- AI: runs n rollouts for each move and pick whatever move lead to the
-- best average score. 
-- Cheats because it knows the full game state (all players hands)
basicAi :: Atout -> Int -> Game -> Player -> [Card] -> IO Card
basicAi atout n game player legalcards = do 
  cardscores <- simul atout n game player legalcards
  pure $ bestmove cardscores


-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Derived from algo2 in:
--  Furtak & Buro : Recursive Monte Carlo Search for Imperfect Information Games
iimcAi :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
iimcAi atout ngames nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> (do 
    possiblegame <- samplePossibleGame pogame player remcards
    simul atout nsim possiblegame player legalcards) -- :: [[(Card, Double)]]
  -- sum the scores for each card accross the n simulated games
  let cardscores =
        (\x -> (fst $ head x, sum $ snd <$> x))
        <$> transpose allscores :: [(Card,Double)] in 
    pure $ bestmove cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player
