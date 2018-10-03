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
import Coinche.Cli
import Options.Applicative
import Data.Semigroup ((<>))



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

runCoinche :: (Ai,Ai,Ai,Ai) -> Trump -> Game -> IO Game
runCoinche playerAIs trump = runGame getCurrentPlayer coincheOver playermove playmove
  where getCurrentPlayer game = head $ _gJoueursRestants game
        playermove = playerMoveCoinche playerAIs trump
        playmove = jouerCarte' (A Heart)

playACoinche :: (Ai,Ai,Ai,Ai) -> IO (Int, Int)
playACoinche playerAIs = do
  hands <- distribuerCartes
  startwith <- getStdRandom (randomR (0, 3)) -- who starts ?
  let players = take 4 $ drop startwith ( cycle [P_1 .. P_4] ) 
      game = initGame{_gJoueursRestants = players, _gPlayersHands = hands}
      trump = A Heart
  finalState <- runCoinche playerAIs trump game
  let s1 = score P_1 trump finalState
      s2 = score P_2 trump finalState
  print (s1,s2,s1+s2)
  pure (s1,s2)

-- Call player's AI and compute the next player move 
playerMoveCoinche :: (Ai,Ai,Ai,Ai) -> Trump -> Game -> Player -> IO Card
playerMoveCoinche (p1ai, p2ai, p3ai, p4ai) trump game player
  | player == P_1 = p1ai game player trump (legalMoves game player)
  | player == P_2 = p2ai game player trump (legalMoves game player)
  | player == P_3 = p3ai game player trump (legalMoves game player)
  | player == P_4 = p4ai game player trump (legalMoves game player)
  
main' :: Options -> IO ()
main' options = do
  ret <- forM [1..n] $ \_ -> playACoinche (p1,p2,p3,p4)
  let v = countVictories ret
      s1 = sum $ fst <$> ret
      s2 = sum $ snd <$> ret
  putStrLn $ "team1 AI : " ++ show (_oT1Ai options)
  putStrLn $ "team2 AI : " ++ show (_oT2Ai options)
  putStrLn $ "Victories " ++ show v
  putStrLn $ "scores " ++ show (fromIntegral s1 / fromIntegral n) ++ " " ++ show (fromIntegral s2 / fromIntegral n)
				     

				     		     

  where n  = _oNRounds options 
        p1 = playerAi options P_1
        p2 = playerAi options P_2
        p3 = playerAi options P_3
        p4 = playerAi options P_4

main = do
  main' =<< execParser options
    where
    options = info (opts <**> helper)
      ( fullDesc
     <> progDesc "French Belote Player (actually Coinche).")
