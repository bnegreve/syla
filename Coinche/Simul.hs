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
import Coinche.Ai
import Coinche.AiMcts

-- Generic function that simulates AI players and make them play against eachother
-- Recursively calls iteself until the game is over and returns the final state.
-- TODO: add a function to compute the list of legal moves and pass the list
-- of legal moves to the AI function
runGame :: (s -> Player) -- a function that returns the next player to play in s
          -> (s -> Bool) -- a function that returns true iff s is terminal
          -> (s -> Player -> IO m) -- AI function, gives the player next move
          -> (s -> m -> s) -- a function that plays the move and return a new state
          -> s -- the current state
          -> IO s -- the final state at the end of the game
runGame getCurrentPlayer terminalP getPlayerMove playMove game
  | terminalP game = pure game
  | otherwise = do
      let curPlayer = getCurrentPlayer game
      move <- getPlayerMove game curPlayer
      runGame
        getCurrentPlayer
        terminalP
        getPlayerMove
        playMove
        (playMove game move)

-- Returns the list of legal moves for player in a given state
legalMoves :: Game -> Player -> [Card]
legalMoves game player = validMoves (A Heart) game (_gPlayersHands game A.! player)

-- COINCHE 


-- Counts the number of victories
-- For now, a victory for team1 is simply when team1 has more points than team2
countVictories :: [(Int, Int)] -> (Int, Int)
countVictories scores = foldl
  (\b a -> if (fst a) > (snd a) then
                (fst b + 1, snd b)
              else
                (fst b, snd b + 1))
  (0,0) scores


-- true if the game is over
coincheOver :: Game -> Bool
coincheOver game =  and $ null . view _w <$> A.elems (_gPlayersHands game)
  
-- runGame specialized for coinche
runCoinche = runGame getCurrentPlayer coincheOver getPlayerMove playMove
  where getCurrentPlayer game = head $ _gJoueursRestants game
        getPlayerMove = getPlayerMoveCoinche
        playMove = jouerCarte' (A Heart)

playACoinche = do
  hands <- distribuerCartes
  startwith <- getStdRandom (randomR (0, 3)) -- who starts ?
  let players = take 4 $ drop startwith ( cycle [P_1 .. P_4] ) 
      game = initGame{_gJoueursRestants = players, _gPlayersHands = hands}
      atout = A Heart
  finalState <- runCoinche game
  let s1 = score P_1 atout finalState
      s2 = score P_2 atout finalState
  print (s1,s2,s1+s2)
  pure (s1,s2)

-- Call player's AI and compute the next player move 
getPlayerMoveCoinche :: Game -> Player -> IO Card
getPlayerMoveCoinche game player
  | player == P_1 || player == P_3 =  
--      dumbAi (A Heart) 1 game player (legalMoves game player)
      mctsAi (A Heart) 10 40 10 game player (legalMoves game player)
--      iimcAi (A Heart) 10 10 game player (legalMoves game player)
  | otherwise =
--      dumbAi (A Heart) 1 game player (legalMoves game player)
--    basicAi (A Heart) 1 game player (legalMoves game player)
--    iimcAi (A Heart) 10 10 game player (legalMoves game player)
      mctsAi (A Heart) 10 10 10 game player (legalMoves game player)

main = do
  args <- getArgs
  let n = read (args !! 0)::Int
  do
    ret <- forM [1..n] $ \_ -> playACoinche
    let v = countVictories ret
        s1 = sum $ fst <$> ret
        s2 = sum $ snd <$> ret
    print $ "Victories " ++ show v
    print $ "Average scores " ++ show (fromIntegral s1 / fromIntegral n,
                                      fromIntegral s2 / fromIntegral n)
