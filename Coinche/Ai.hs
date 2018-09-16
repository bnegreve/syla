module Coinche.Ai where

import Data.Maybe
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
import qualified Data.Map as M
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
buildRandomHands :: Game -> [Card] -> [(Player,Int)] -> [(Player,Hand)]
buildRandomHands game remainingCards sizes = fst $ foldr f ([],remainingCards) sizes
  where f (player,size) (hands,remCards) = let (hand, rest) = splitAt size remCards
                                  in ((player,Hand hand) :hands, rest)
{- fonction violente -}
buildRandomHands' :: Game
                  -> [Card] -- remaining cards
                  -> [(Player, Int)] -- list of hand sizes for each player
                  -> Maybe [(Player, Hand)] -- list of player hands
buildRandomHands' game remcards sizes
  | isNothing result = Nothing
  |otherwise = Just [(pi,hi) | (pi, hi@(Hand h)) <- A.assocs $ fst $ fromJust result,
                                                  not $ null h ]
               
  where players = fst <$> sizes
        initialHands :: A.Array Player Hand
        initialHands = A.listArray (P_1,P_4) $ take 4 (repeat $ Hand [])
        initialSizes = A.listArray (P_1,P_4) (take 4 (repeat 0)) A.// sizes
        result = foldl f (Just (initialHands, initialSizes)) remcards
        f Nothing _ = Nothing
        f (Just (hands, remsizes)) card =
          let playercandidates = [ pi | -- pi <- players,
                                       pi <- fst <$> sortBy (\(pi,cs) (pi', cs') -> length cs `compare` length cs') (M.toList playersPossibleColors),
                                      remsizes A.! pi > 0, -- player  hand is not full
                                      playerCanHaveColor game pi (_cColor card)]
              fstplayer = head playercandidates
              fstplayerhand = view _w $ hands A.! fstplayer
              playersPossibleColors :: M.Map Player [Color]
              playersPossibleColors = M.fromList [(pi,possibleColors) |
                                                   pi <- players,
                                                   let possibleColors = [color | color <- colors, playerCanHaveColor game pi color]]
          in case playercandidates of
            [] -> Nothing
            otherwise -> Just (hands A.// [(fstplayer, Hand $ card:fstplayerhand)],
                               remsizes A.// [(fstplayer, remsizes A.! fstplayer - 1)] )


-- -- Build a player hands that is consistant with his/her previous actions 
playerPossibleCards :: Game -> Player -> [Card] -> [Card]
playerPossibleCards game player remcards = [ci | ci <- remcards,
                                            _cColor ci `elem` possiblecolors]
 where prevturns = _gPlisJoues game
       possiblecolors = [color | color <- colors,
                                 playerCanHaveColor game player color]

-- Returns True if player has always followed with the color
playerCanHaveColor :: Game -> Player -> Color -> Bool
playerCanHaveColor game player color = and $ playerPlayedColor player <$> turnscolor
  where turns = (_gPliCourant game) : (fst <$> _gPlisJoues game)
        turnscolor = [ti | ti <- turns,
                           isJust $ turnColor ti,
                           fromJust (turnColor ti) == color]

-- Returns True if player has played color in turn 
playerPlayedColor :: Player -> Pli -> Bool 
playerPlayedColor player t@(Pli turn)
  | isNothing turncolor = True
  | null playercards = True
  | otherwise = _cColor (head playercards) == fromJust turncolor
  where turncolor = turnColor t
        playercards = [ci | (ci,pi) <- turn, pi == player]

-- Return the color of a particular turn or Nothing if turn is empty so far
turnColor :: Pli -> Maybe Color
turnColor (Pli []) = Nothing
turnColor (Pli turn) = Just $ _cColor $ fst $ head turn
  

-- Returns the partially observed game from player's perspective
partiallyObservedGame :: Player -> Game -> Game
partiallyObservedGame p g = 
  g { _gPlayersHands =
        _gPlayersHands g A.// [(pi, Hand []) | pi <- [P_1 .. P_4], pi /= p]  }

-- Sample a possible game that is compatible with the current player game
-- Warning: playergame must be a partially observed game (obtained with playerGame)
samplePossibleGame :: Game -> Player -> [Card] -> IO Game
samplePossibleGame g p remcards =
  pure $ g & gPlayersHands %~ (A.// randomhands)
  where phandsize = length $ view _w $ _gPlayersHands g A.! p
        mustPlay = _gJoueursRestants g \\ [p]
        havePlayed =  (snd <$> (g ^. gPliCourant . _w)) \\ [p]
        randomhands = buildRandomHands g remcards $ 
                      [(pi,phandsize) | pi <- mustPlay]
                      ++ [(pi,phandsize - 1) | pi <- havePlayed]

-- Sample a possible game that is compatible with the current player game
-- Warning: playergame must be a partially observed game (obtained with playerGame)
samplePossibleGameSmart :: Game -> Player -> [Card] -> IO (Maybe Game)
samplePossibleGameSmart g p remcards
  | isNothing randomhands = pure Nothing
  | otherwise = pure $ Just $ g & gPlayersHands %~ (A.// fromJust randomhands)
  where phandsize = length $ view _w $ _gPlayersHands g A.! p
        mustPlay = _gJoueursRestants g \\ [p]
        havePlayed =  (snd <$> (g ^. gPliCourant . _w)) \\ [p]
        randomhands = buildRandomHands' g remcards $ 
                      [(pi,phandsize) | pi <- mustPlay]
                      ++ [(pi,phandsize - 1) | pi <- havePlayed]



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
iimcAiSmart :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
iimcAiSmart atout ngames nsim game player legalcards = do
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


-- AI: sample possible games from current game state and
-- runs simulations inside the possibles games
-- Warning: In this versions the games are sampled without analyzing
-- the previous turns
iimcAi :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
iimcAi atout ngames nsim game player legalcards = do
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
