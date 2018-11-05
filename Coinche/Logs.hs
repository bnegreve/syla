module Coinche.Logs where

import Coinche.Engine
import Control.Lens
import Data.Binary
import Data.List
import Data.Binary.Get
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Maybe

type Entry =  (Game,GameStats)
type Logs = [Entry]

readLogs :: IO Logs
readLogs = runGet decodeLogs <$> B.readFile "games.log"

select :: Int -> IO Entry
select i = (!! i) <$> readLogs

game i = fst <$> select i

stats :: Int -> IO GameStats
stats i = snd <$> select i

lookupDistribs :: Entry -> Player -> [Distribution]
lookupDistribs (_, GameStats m) p =
  fromJust $ M.lookup p m

decodeLogs :: Get Logs
decodeLogs = do
        empty <- isEmpty
        if empty then pure []
         else do
                 r <- get
                 rest <- decodeLogs
                 pure $ r:rest


hand :: Entry -> Player -> Hand
hand (game,_) player = Hand [c | (Pli pli,gagnant)  <- _gPlisJoues game, (c,pl) <- pli, pl == player]
hands entry = [hand entry pi | pi <- [P_1,P_2,P_3,P_4]]


formatEntry curplayer = do
        logs <- readLogs
        writeFile "dataset.csv" $ genCsv [show $ fromEnum <$> formatGameState curplayer entry| entry <- logs]
        writeFile "output.csv" $ genCsv [show $ fromEnum <$> formatStats curplayer entry| entry <- logs]
--        writeFile "testinputset.csv" $ unlines [show (fromEnum <$> formatGameState curplayer entry)| entry <- logs]
--        writeFile "testoutputset.csv" $ unlines [show (fromEnum <$> formatStats curplayer entry)| entry <- logs]
 where   genCsv :: [String] -> String
         genCsv l = unlines $ init . tail <$> l


formatStats curplayer entry = tensor
  where distrib = lookupDistribs entry curplayer !! 0
        tensor = [if null estimations then 0 else head estimations | ci <- allCards, let estimations = [ej | (cj,ej) <- distrib, cj == ci]]


{- retourne une ligne contenant la main de curplayer concaténée avec les 32*3 vecteurs correspondant aux coups joués par les autres au PLI 1 -}
formatGameState curplayer entry  = concat [formatHand entry curplayer, formatPliObserve curplayer entry 1]

{- Retourne le pli i de l'entrée -}
pli :: Entry -> Int -> Pli
pli (game, _) i = let (pl,_) = _gPlisJoues game  !! (i-1)
                  in pl

{- Retourne les cartes jouées par les autres joueurs -}
formatPliObserve :: Player -> Entry -> Int -> [Bool]
formatPliObserve playercur entry i = concat [formatPliPlayer entry i player | player <- [P_1 .. P_4], player /= playercur]

{- Un vecteur de 32 cartes où le x = 1 si player a joué la carte dans le pli i -}
formatPliPlayer entry i player = tensor
  where (Pli pl) = pli entry i
        cartesJouees = [ci | (ci,playerj) <- pl, playerj == player]
        tensor = concat [fmap (\x -> x `elem` cartesJouees) allCards ]
{- Un vecteur de 32 cartes ou x = 1 si le player a la carte en main -}
formatHand entry player = tensor
  where (Hand h) = hand entry player
        tensor = fmap (\x -> x `elem` h) allCards

