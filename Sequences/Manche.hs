{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Coinche.Sequences.Manche where

import Coinche.Game
import Coinche.Types
import Coinche.Rules
import Coinche.Parser
import Data.List
import Control.Lens
import GHC.Generics
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Text as T

import qualified Data.Array as A
import Data.Ix
import Data.Maybe
import IRC
import Network.IRC.Client
import Debug.Trace

data Retour = Continuing | Finished | WrongEntry | Discarded

initPlayerList = A.listArray (P_1,P_4) $ take 4 $ repeat ""

data AnnonceManager = AnnonceManager { _mBest :: Maybe (Annonce, T.Text), _mPlayers :: [T.Text]}
makeLenses ''AnnonceManager
initAnnonceManager = AnnonceManager Nothing []

data GameManager = GameManager {_gmGame :: Game, _gmPlayers :: A.Array Player T.Text, _gmAtout :: Atout}
makeLenses ''GameManager
initGameManager = GameManager initGame initPlayerList (A Club)

type AnnonceManagerT = State AnnonceManager
type GameManagerT = State GameManager




announceHandler ::  T.Text -> T.Text -> T.Text -> AnnonceManagerT Retour
announceHandler nick chan str = do
        currentPlayer <- head <$> use mPlayers
        best <- use mBest
        
        if currentPlayer /= nick then pure Discarded
                                 -- TODO si tout le monde a passé, arrêter la game

                                 -- Si c'est l'auteur de la meilleure annonce qui passe, on arrête les enchères
          else if "!p" `T.isInfixOf` str && isJust best && snd (fromJust best) == currentPlayer then pure Finished
                                 -- Sinon on passe au joueur suivant
          else if "!p" `T.isInfixOf` str  then mPlayers %= tail >>  pure Continuing
          else case (best,readAnnonce str) of
                    (_ ,Nothing) -> pure WrongEntry
                    (Nothing, Just ann) -> mBest .= Just (ann,currentPlayer) >> mPlayers %= tail >>  pure Continuing
                    (Just bestAnn, Just ann) -> let newBestP = fst bestAnn < ann in do
                                               if newBestP then mBest .= Just (ann,currentPlayer) >> mPlayers %= tail >> pure Continuing
                                                           else pure WrongEntry
gameHandler :: T.Text -> T.Text -> T.Text -> GameManagerT Retour
gameHandler nick chan str = do
        game <- use gmGame
        atout <- use gmAtout
        let currentPlayer = head $ _gJoueursRestants game
            cardM = readCard str
        playersMap <- use gmPlayers
        if playersMap A.! currentPlayer /= nick then pure Discarded    
         else if isJust cardM then do
                 let c = fromJust cardM 
                 jouableP <- zoom gmGame $ jouerP atout c
                 if not jouableP then pure WrongEntry
                   else do 
                           finPliP <- null <$> use (gmGame . gJoueursRestants)
                           if not finPliP then pure Continuing
                                  else do -- Le pli est fini, on check si c'était le dernier pli
                                          plusDeCarteP <- null <$> use (gmGame . gPlayersHands . ix P_1 . _w )
                                          if plusDeCarteP then
                                                          -- Fin de la manche, il ne reste plus de cartes
                                                          pure Finished
                                            else do -- Le pli est fini
                                                    -- On determine qui l'a emporté
                                               pli <- use $ gmGame . gPliCourant
                                               let (card, winner) = foldl1 (\acc el -> case comparerCarte atout (fst acc) (fst el) of GT -> acc
                                                                                                                                      LT -> el) (view _w pli) -- LA CARTE INITIALE EST LA PREMIERE
                                               -- On initialise le nouveau plu
                                               trace (show card) $ gmGame . gPliCourant .= Pli []
                                               gmGame . gPlisJoues %= ((pli,winner):)
                                               
                                                -- Le joueur qui commence est celui qui a gagné le pli précédetn
                                               gmGame . gJoueursRestants .= take 4 (dropWhile (/= winner) $ cycle [P_1, P_2, P_3, P_4])
                                               pure Continuing

                                                
         else  pure Discarded




                    


   


