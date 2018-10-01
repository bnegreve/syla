module Coinche.Cli where

import Options.Applicative
import Data.Semigroup ((<>))
import Coinche.Types
import Coinche.Game
import Coinche.AiMcts
import Coinche.Ai

data Options = O { _oNRounds    :: Int
                 , _oTeam1      :: String
                 , _oTeam2      :: String
                 }

data AiOpts = GenericAiOpts | MctsOpts

type Ai = Game -> Player -> Atout -> [Card] -> IO Card
  
playerAi :: Options -> Player -> Ai 
playerAi options player 
  | ainame == "dumbAi" = dumbAi
  | ainame == "basicAi" = basicAi (GAO "basicAi" 10 10)
  | ainame == "iimcAi" = iimcAi (GAO "iimcAi" 10 10)  
  where ainame = playerAiName options player 

playerAiName :: Options -> Player -> String
playerAiName options player
  | player == P_1 || player == P_3 = _oTeam1 options 
  | otherwise = _oTeam2 options

opts :: Parser Options
opts = O
      <$> option auto
          ( long "nrounds"
         <> short 'n'
         <> metavar "N"
         <> value 10
         <> showDefault
         <> help "Number of rounds to be played." )
      <*> strOption
          ( long "team1"
         <> short '1'
         <> metavar "AI-NAME"
         <> value "iimcAi"
         <> showDefault
         <> help "Ai for team 1.")  
      <*> strOption
          ( long "team2"
         <> short '2'
         <> metavar "AI-NAME"
         <> value "dumbAi"
         <> showDefault
         <> help "Ai for team 2." )
