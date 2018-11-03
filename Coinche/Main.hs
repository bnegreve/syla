module Coinche.Main where


import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Engine
import Coinche.Ai.Tools
import Coinche.Ai.BasicAi
import Coinche.Ai.AiMcts

import System.Random.Shuffle
import System.Random
import Data.List
import qualified Data.Map as M
import qualified Data.Array as A
import System.Environment 
import Coinche.Cli
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.Reader
import Data.Binary
import qualified Data.ByteString.Lazy as B

-- Generic function that simulates AI players and make them play against eachother
-- Recursively calls iteself until the game is over and returns the final state.
-- TODO: add a function to compute the list of legal moves and pass the list
-- of legal moves to the AI function
runGame :: (s -> Player) -- a function that returns the next player to play in s
          -> (s -> Bool) -- a function that returns true iff s is terminal
          -> (s -> Player -> IO (m, GameStats) ) -- AI function, gives the player next move
          -> (s -> m -> s) -- a function that plays the move and return a new state
          -> s -- the current state
          -> IO (s, GameStats) -- the final state at the end of the game and gametats 
runGame getCurrentPlayer terminalP getPlayerMove playMove game
  | terminalP game = pure (game,mempty)
  | otherwise = do
      let curPlayer = getCurrentPlayer game
      (move, gamestats) <- getPlayerMove game curPlayer
      (finalState,finalGameStats) <- runGame
        getCurrentPlayer
        terminalP
        getPlayerMove
        playMove
        (playMove game move)
      pure $ (finalState, mappend gamestats finalGameStats)

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

runCoinche :: (Ai,Ai,Ai,Ai) -> Bid -> Game -> IO (Game,GameStats)
runCoinche playerAIs bid = runGame getCurrentPlayer coincheOver playermove playmove
  where getCurrentPlayer game = head $ _gJoueursRestants game
        playermove = playerMoveCoinche playerAIs bid
        playmove = playCard $ _bColor bid

playACoinche :: (Ai,Ai,Ai,Ai) -> IO (Int, Int)
playACoinche playerAIs = do
  (bid@(Bid _ trump) , game) <- startCoinche
  (finalState,stats) <- runCoinche playerAIs bid game
  let s1 = score P_1 trump finalState
      s2 = score P_2 trump finalState
  --B.appendFile "games.log" $ encode finalState
--  logs <- decode <$> B.readFile "games2.log" :: IO [(Game,GameStats)]
  B.writeFile "games.log" $ encode $ (finalState,stats)
  print (s1,s2,s1+s2)
  pure (s1,s2)



-- Call player's AI and compute the next player move 
--playerMoveCoinche :: (Ai,Ai,Ai,Ai) -> Bid -> Game -> Player -> IO Card
playerMoveCoinche :: (Ai,Ai,Ai,Ai) -> Bid -> Game -> Player -> IO (Card, GameStats)
playerMoveCoinche (p1ai, p2ai, p3ai, p4ai) bid game player
  | player == P_1 = recordStats <$> runReaderT (p1ai game player (legalMoves game player)) bid
  | player == P_2 = recordStats <$> runReaderT (p2ai game player (legalMoves game player)) bid
  | player == P_3 = recordStats <$> runReaderT (p3ai game player (legalMoves game player)) bid
  | player == P_4 = recordStats <$> runReaderT (p4ai game player (legalMoves game player)) bid
  where
    recordStats :: Distribution -> (Card, GameStats)
    recordStats dist = (bestmove dist, GameStats $ M.fromList [(player,[dist])])

main' :: Options -> IO ()
main' options = do
  ret <- forM [1..n] $ \_ -> playACoinche (p1,p2,p3,p4)
  let v = countVictories ret      
      avg1 = (fromIntegral (sum $ fst <$> ret)) / nf
      stdev1 = sqrt $ (sum $ (\(a,_) -> (fromIntegral a)**2) <$> ret) / nf - avg1**2
      avg2 = fromIntegral (sum $ snd <$> ret) / nf 
      stdev2 = sqrt $ (sum $ (\(_,b) -> (fromIntegral b)**2) <$> ret) / nf - avg2**2
      ci1 = (avg1 - (stdev1 / (sqrt nf)), avg1 + (stdev1 / (sqrt nf)))
      ci2 = (avg2 - (stdev2 / (sqrt nf)), avg2 + (stdev2 / (sqrt nf)))
  putStrLn $ "team1 AI : " ++ show (_oT1Ai options)
  putStrLn $ "team2 AI : " ++ show (_oT2Ai options)
  putStrLn $ "Victories " ++ show v
  putStrLn $ "scorep1 " ++ (show avg1) ++ " " ++ (show stdev1) ++ " " ++ (show ci1)
  putStrLn $ "scorep2 " ++ (show avg2) ++ " " ++ (show stdev2) ++ " " ++ (show ci2)
  where n  = _oNRounds options
        nf = fromIntegral n 
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
