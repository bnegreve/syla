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
import Coinche.Rules
import Data.List
import qualified Data.Array as A


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

getPlayerMoveCoinche :: Game -> Player -> IO Card
getPlayerMoveCoinche game player
  | player == P_1 || player == P_3 =  
    basicAi (A Heart) 1 game player (getLegalMoves game player)
  | otherwise = 
    basicAi (A Heart) 1000 game player (getLegalMoves game player)
    
getLegalMoves :: Game -> Player -> [Card]
getLegalMoves game player = validMoves (A Heart) game (_gPlayersHands game A.! player)

basicAi :: Atout -> Int -> Game -> Player -> [Card] -> IO Card
basicAi atout n game player cards = do 
  ret <- forM cards $ \c -> do
      let scores = [rollout player atout (jouerCarte' atout game c) | i <- [1..n]]
      pure (c, (sum scores) / fromIntegral n)
  let best = maximumBy (\(c,v) (c',v') -> v `compare` v') ret
  pure $ fst best

naiveSimu = simulator getCurrentPlayer terminalP getPlayerMove playMove
  where getCurrentPlayer game = head $ _gJoueursRestants game
        terminalP game = and $ null . view _w <$> A.elems (_gPlayersHands game)
        getPlayerMove = getPlayerMoveCoinche
        playMove = jouerCarte' (A Heart)
simuler = do
  hands <- distribuerCartes
  let g = initGame{_gJoueursRestants = [P_1, P_2, P_3, P_4], _gPlayersHands = hands}
      atout = A Heart
  finalState <- naiveSimu g
  let s1 = score P_1 atout finalState
      s2 = score P_2 atout finalState
  print (s1,s2,s1+s2)
  pure (s1,s2)

main = let testN = 100
       in do
  ret <- forM [1.. testN] $ \_ -> simuler
  let s1 = sum $ fst <$> ret
      s2 = sum $ snd <$> ret
  print (fromIntegral s1 / fromIntegral testN, fromIntegral s2 / fromIntegral testN)
{-
getPlayerMove :: (s -> Player -> [m]) -- provides legal moves of the player from the state
              -> (s -> Player -> [m] -> IO m) -- AI function
              -> s
              -> Player
              -> IO m  
getPlayerMove legalMoves ai game player =  ai game player (legalMoves game player)
-}
-- Coinche
