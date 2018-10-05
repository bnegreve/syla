module Coinche.Cli where

import Options.Applicative
import Data.Semigroup ((<>))
import Coinche.Types
import Coinche.Game
import Coinche.AiMcts
import Coinche.Ai

data Options = O { _oNRounds    :: Int
                 , _oT1Ai      :: String
                 , _oT2Ai      :: String
                 , _oNGames1 :: Int
                 , _oNGames2 :: Int
                 , _oNSim1 :: Int
                 , _oNSim2 :: Int
                 , _oMctsNLoops1 :: Int 
                 , _oMctsNLoops2 :: Int 
                 , _oMctsAlpha1 :: Double
                 , _oMctsAlpha2 :: Double
                 , _oMctsNormalizeScores1 :: Bool
                 , _oMctsNormalizeScores2 :: Bool

                 }

data AiOpts = GenericAiOpts | MctsOpts

type Ai = Game -> Player -> Atout -> [Card] -> IO Card
  
playerAi :: Options -> Player -> Ai 
playerAi options player 
  | ainame == "dumbAi" = dumbAi
  | ainame == "basicAi" = basicAi (GAO "basicAi" ngames nsim)
  | ainame == "iimcAi" = iimcAi (GAO "iimcAi" ngames nsim)  
  | ainame == "iimcAiSmart" = iimcAiSmart (GAO "iimcAi" ngames nsim)
  | ainame == "mctsAi" = mctsAi (MO ngames nloops nsim alpha ns)  
  where (ainame, ngames, nsim) = basicOptions options player
        (nloops, alpha, ns) = mctsOptions options player 

playerAiName :: Options -> Player -> String
playerAiName options player
  | player == P_1 || player == P_3 = _oT1Ai options 
  | otherwise = _oT2Ai options

basicOptions :: Options -> Player -> (String, Int, Int)
basicOptions options player
  | player == P_1 || player == P_3 = (_oT1Ai options, _oNGames1 options, _oNSim1 options)
  | otherwise =  (_oT2Ai options, _oNGames2 options, _oNSim2 options)

mctsOptions :: Options -> Player -> (Int, Double, Bool)
mctsOptions options player
  | player == P_1 || player == P_3 = (_oMctsNLoops1 options,
                                      _oMctsAlpha1 options,
                                      _oMctsNormalizeScores1 options)
  | otherwise = (_oMctsNLoops2 options,
                 _oMctsAlpha2 options,
                 _oMctsNormalizeScores2 options)

opts :: Parser Options
opts = O <$> option auto
          ( long "nrounds"
            <> short 'n'
            <> metavar "N"
            <> value 10
            <> showDefault
            <> help "Number of rounds to be played." )
          <*> strOption
          ( long "team1"
            <> short 't'
            <> metavar "AI-NAME"
            <> value "iimcAiSmart"
            <> showDefault
            <> help "Ai for team 1.")
          <*> strOption
          ( long "team2"
            <> short 'T'
            <> metavar "AI-NAME"
            <> value "dumbAi"
            <> showDefault
            <> help "Ai for team 2." )
          <*> option auto
          ( long "ngames1"
            <> short 'g'
            <> metavar "N"
            <> short 'g'
            <> value 10
            <> showDefault
            <> help "Number of games for team 1 AI." )
          <*> option auto
          ( long "ngames2"
            <> short 'G'
            <> metavar "N"
            <> value 10
            <> showDefault
            <> help "Number of games for team 2 AI." )
          <*> option auto
          ( long "nsim1"
            <> short 's'
            <> value 1
            <> metavar "N"
            <> showDefault
            <> help "Number of sims for team 1 AI." )
          <*> option auto
          ( long "nsim2"
            <> short 'S'
            <> metavar "N"
            <> value 1
            <> showDefault
            <> help "Number of sims for team 2 AI." )
          <*> option auto
          ( long "nloops1"
            <> short 'l'
            <> metavar "N"
            <> value 10
            <> showDefault
            <> help "Number of loops for team 1 mcts AI." )
          <*> option auto
          ( long "nloops2"
            <> short 'L'
            <> metavar "N"
            <> value 10
            <> showDefault
            <> help "Number of loops for team 2 mcts AI." )
          <*> option auto
          ( long "alpha1"
            <> short 'a'
            <> metavar "N"
            <> value 0.7
            <> showDefault
            <> help "alpha for team 1 mcts AI." )
          <*> option auto
          ( long "alpha2"
            <> short 'A'
            <> metavar "N"
            <> value 0.4
            <> showDefault
            <> help "alpha for team 2 mcts AI." )
          <*> option auto
          ( long "normalizescores1"
            <> short 'z'
            <> metavar "N"
            <> value False
            <> showDefault
            <> help "Normalize score flag for team 1 mcts AI." )
          <*> option auto
          ( long "normalizescores2"
            <> short '2'
            <> metavar "N"
            <> value False -- not good
            <> showDefault
            <> help "Normalize score flag for team 2 mcts AI." )
