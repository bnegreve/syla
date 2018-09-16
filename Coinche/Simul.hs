module Coinche.Simul where


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

simulator :: (s -> Player) -- get current player from state
          -> (s-> Bool)    -- true if the current state is terminal
          -> (s -> Player -> IO m) -- gives the player move
          -> (s -> m -> s) -- plays the move
          -> s -- current state
          -> IO s -- final state
simulator getCurrentPlayer terminalP getPlayerMove playMove game
  | terminalP game = pure game
  | otherwise = do
      let curPlayer = getCurrentPlayer game
      move <- getPlayerMove game curPlayer
      simulator
        getCurrentPlayer
        terminalP
        getPlayerMove
        playMove
        (playMove game move)

-- triggers one AI to compute next player move
getPlayerMoveCoinche :: Game -> Player -> IO Card
getPlayerMoveCoinche game player
  | player == P_1 || player == P_3 =  
    dumbAi (A Heart) 1 game player (legalMoves game player)
  | otherwise = 
--    basicAi (A Heart) 1 game player (legalMoves game player)
    iimcAi (A Heart) 10 100 game player (legalMoves game player)
    
legalMoves :: Game -> Player -> [Card]
legalMoves game player = validMoves (A Heart) game (_gPlayersHands game A.! player)

-- simply returns the best move in a list of tuples (moves, scores)
bestmove :: [(m, Double)] -> m
bestmove movescores =
  let max = maximumBy (\(m,s) (m',s') -> s `compare` s') movescores in
    fst max

-- COINCHE 

-- Runs n rollouts for each possible card and returns the average
-- score for each card.
simul :: Atout -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
simul atout n game player cards = do
  forM cards $ \c -> do
    cardscores <- forM [1..n] $ \_ -> do
      rollout player atout (jouerCarte' atout game c) 
    let cardavgscore = sum cardscores / fromIntegral n
    pure (c, cardavgscore)

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

runGame = simulator getCurrentPlayer terminalP getPlayerMove playMove
  where getCurrentPlayer game = head $ _gJoueursRestants game
        terminalP game = and $ null . view _w <$> A.elems (_gPlayersHands game)
        getPlayerMove = getPlayerMoveCoinche
        playMove = jouerCarte' (A Heart)

playAGame = do
  hands <- distribuerCartes
  startwith <- getStdRandom (randomR (0, 3)) -- who starts ?
  let players = take 4 $ drop startwith ( cycle [P_1 .. P_4] ) 
      game = initGame{_gJoueursRestants = players, _gPlayersHands = hands}
      atout = A Heart
  finalState <- runGame game
  let s1 = score P_1 atout finalState
      s2 = score P_2 atout finalState
  print (s1,s2,s1+s2)
  pure (s1,s2)

countVictories :: [(Int, Int)] -> (Int, Int)
countVictories scores = foldl
  (\b a -> if (fst a) > (snd a) then
                (fst b + 1, snd b)
              else
                (fst b, snd b + 1))
  (0,0) scores

main = do
  args <- getArgs
  let n = read (args !! 0)::Int
  do
    ret <- forM [1..n] $ \_ -> playAGame
    let v = countVictories ret
        s1 = sum $ fst <$> ret
        s2 = sum $ snd <$> ret
    print $ "Victories " ++ show v
    print $ "Average scores " ++ show (fromIntegral s1 / fromIntegral n,
                                      fromIntegral s2 / fromIntegral n)
