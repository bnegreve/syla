{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Coinche.Engine.Game where

import Debug.Trace
import Coinche.Engine.Rules
import Coinche.Engine.Types
import Data.List
import Control.Lens
import GHC.Generics
import Control.Monad
import Control.Monad.State
import qualified Data.Array as A
import Data.Ix
import Data.Maybe
import System.Random.Shuffle
newtype Pli = Pli [(Card,Player)]
    deriving Generic
instance Wrapped Pli

{- Une manche après que les annonces soient faites -}
data Game = Game { _gPlisJoues :: [(Pli,Player)],
                   _gPliCourant :: Pli,
                   -- Les joueurs dans l'ordre de jeu
                   _gJoueursRestants :: [Player],
                   _gPlayersHands :: A.Array Player Hand
                   }
makeLenses ''Game

-- dummy Game creator
initGame = Game [] (Pli []) [] (A.listArray (P_1,P_4) $ repeat $ Hand [])

type GameT m = StateT Game m

jouerCarte :: (Monad m) => Card -> GameT m ()
jouerCarte c = do
     curPlayer <- head <$> use gJoueursRestants   
     gPliCourant . _w %= (++ [(c, curPlayer)] ) 
     gJoueursRestants %= tail
couleurDemandee p = RoundColor $ _cColor $ head p

removeCardFromHand :: (Monad m) => Player -> Card -> GameT m Bool
removeCardFromHand player c = do
        h <- use (gPlayersHands . ix player . _w) 
        let coupPossedéP = c `elem` h  
        if not coupPossedéP then pure False
             else  do gPlayersHands . ix player . _w %= delete c
                      pure True


jouerP :: (Monad m) => Atout -> Card-> GameT m Bool
jouerP a c = do
        curPli <- use gPliCourant
        curPlayer <- head <$> use gJoueursRestants
        handT <- use $ gPlayersHands
        let round = fst <$> ( view _w $ curPli)
            hand = handT A.! curPlayer
            possibilites = if null round then view _w hand else coupsPossibles a (couleurDemandee round) (coequipierP a curPlayer curPli) round hand
            jouableP = c `elem` possibilites
        case trace (show (if not $ null round then Just (couleurDemandee round) else Nothing, possibilites)) $ jouableP of
            False -> do -- le coup joué est impossible
                        pure False
            True -> do -- le coup joué est possible, on le joue et c'est le tour suivant   
                       coupRetireP <- removeCardFromHand curPlayer c
                       if not coupRetireP then pure False  -- le joueur ne possede pas la carte
                           else do
                                       jouerCarte c
                                       pure True


meilleureCarteDuPli :: Atout -> [(Card,Player)] -> Maybe (Card,Player)
meilleureCarteDuPli _ [] = Nothing
meilleureCarteDuPli a l = pure $ foldl1 f l  -- LA CARTE INITIALE EST LA PREMIERE
 where f acc el = case comparerCarte a (fst acc) (fst el) of
                       GT -> acc
                       LT -> el
coequipierP a currentPlayer (Pli pl)
    | null pl = False
    | otherwise = coequipier (snd $ fromJust $ meilleureCarteDuPli a pl) == currentPlayer

coequipier P_1 = P_3
coequipier P_2 = P_4
coequipier P_3 = P_1
coequipier P_4 = P_2
compterPlis :: (Monad m) => Atout -> GameT m Int
compterPlis atout = do
        plisJoues <-  use gPlisJoues
        let plisValeurs = fmap f plisJoues 
            f (Pli p, winner) = (valeurPli atout $ fst <$> p , winner)
            (team1,team2) = foldl (\(counter1,counter2) (valeur,winner) -> if winner == P_1 || winner == P_3 then (valeur + counter1,counter2) else (counter1,valeur+counter2)) (0,0) plisValeurs
        undefined    


