{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings#-}
module Coinche.Engine.Rules where

import Debug.Trace
import Coinche.Engine.Types
import System.Random.Shuffle
import Data.List
import Control.Lens
import GHC.Generics
import qualified Data.Text as T


trumpToInt :: Rank -> Int
trumpToInt x = case x of
  C_J -> 8
  C_9 -> 7
  C_As ->6
  C_10 -> 5
  C_K -> 4
  C_Q -> 3
  C_8 -> 2
  C_7 -> 1

intToTrump :: Int -> Rank
intToTrump x
  |x == 8 = C_J
  | x == 7 = C_9
  | x == 6 = C_As
  | x == 5 = C_10
  | x == 4 =C_K
  | x == 3 = C_Q
  | x == 2 = C_8
  | x == 1 = C_7
  | otherwise = error $ "intToTrump" ++ show x
  
       

normalToInt x = 1+fromEnum x

compareTrumpsValue x y = trumpToInt x `compare` trumpToInt y

-- LT si la cartedemandée est meilleure que la carte jouée
-- GT si la carte jouée gagne

comparerCarte :: Atout -> Card -> Card -> Ordering
comparerCarte (A ToutAt) carteDemandee carteJouee 
    | couleurDemandee == couleurJouee = compareTrumpsValue valDemandee valJouee
    | couleurDemandee /= couleurJouee = LT
   where couleurDemandee = _cColor carteDemandee
         couleurJouee = _cColor carteJouee
         valDemandee = _cRank carteDemandee
         valJouee = _cRank carteJouee


    
comparerCarte (A SansAt) carteDemandee carteJouee 
    | couleurDemandee == couleurJouee = normalToInt valDemandee `compare` normalToInt valJouee
    | couleurDemandee /= couleurJouee = LT
   where couleurDemandee = _cColor carteDemandee
         couleurJouee = _cColor carteJouee
         valDemandee = _cRank carteDemandee
         valJouee = _cRank carteJouee

   
comparerCarte (A atout) carteDemandee carteJouee
    | atout == couleurDemandee && atout /= couleurJouee = GT
    | atout /= couleurDemandee && atout == couleurJouee = LT
    | atout == couleurDemandee && atout == couleurJouee = compareTrumpsValue valDemandee valJouee
    | couleurDemandee /= couleurJouee = GT
    | atout /= couleurDemandee && atout /= couleurJouee = normalToInt valDemandee `compare` normalToInt valJouee 
  where couleurDemandee = _cColor carteDemandee
        couleurJouee = _cColor carteJouee
        valDemandee = _cRank carteDemandee
        valJouee = _cRank carteJouee

           
allCards = [Card r c | r <- all, c <- [Club,Spike,Diamond,Heart] ]
  where
    all :: (Enum a) => [a]
    all = enumFrom (toEnum 0)
shuffleCards :: IO [Card]
shuffleCards = shuffleM allCards

{- Retourne les coups possibles pour une main et une liste de coups joués.
                         NE GERE PAS REGLE DU SUB
-}
coupsPossibles :: Atout -> RoundColor -> Bool ->  [Card] -> Hand -> [Card]
coupsPossibles _ _ _ _ (Hand []) = []
coupsPossibles (A ToutAt) (RoundColor rcolor) _ plays (Hand cards)
    | not $ null atoutsAuDessus = atoutsAuDessus
    | not $ null atoutsPossibles = atoutsPossibles
    | otherwise = resteDesCartes
  where atout = rcolor
        atoutsJoues = [ci | ci <- plays, _cColor ci == atout]
        meilleurAtoutJoue = intToTrump $ maximum $ trumpToInt . _cRank <$> atoutsJoues
        cartesDeLaCouleur = [ci | ci <- cards, _cColor ci == rcolor]
        meilleurAtoutQue a = [ci | ci <- cards , _cColor ci == atout, trumpToInt (_cRank ci) > trumpToInt a]
        resteDesCartes = [ci | ci <- cards, _cColor ci /= rcolor, _cColor ci /= atout]
        atoutsPossibles
                | null atoutsJoues || null atoutsAuDessus = [ci | ci <- cards, _cColor ci == atout] 
                | otherwise = meilleurAtoutQue meilleurAtoutJoue

        atoutsAuDessus = meilleurAtoutQue meilleurAtoutJoue

coupsPossibles (A SansAt) (RoundColor rcolor) _ plays (Hand cards)
    | null cartesDeLaCouleur = resteDesCartes
    | otherwise = cartesDeLaCouleur
  where atout = rcolor
        atoutsJoues = [ci | ci <- plays, _cColor ci == atout]
        meilleurAtoutJoue = intToTrump $ maximum $ trumpToInt . _cRank <$> atoutsJoues
        cartesDeLaCouleur = [ci | ci <- cards, _cColor ci == rcolor]
        meilleurAtoutQue a = [ci | ci <- cards , _cColor ci == atout, trumpToInt (_cRank ci) > trumpToInt a]
        resteDesCartes = [ci | ci <- cards, _cColor ci /= rcolor, _cColor ci /= atout]
        atoutsPossibles
                | null atoutsJoues || null atoutsAuDessus = [ci | ci <- cards, _cColor ci == atout] 
                | otherwise = meilleurAtoutQue meilleurAtoutJoue

        atoutsAuDessus = meilleurAtoutQue meilleurAtoutJoue

coupsPossibles (A atout) (RoundColor rcolor) coequipierMaitreP plays (Hand cards) 
    -- Si on est le premier joueur, on fait ce qu'on veut
    | null plays = cards

    {- Si on commence à l'atout et qu'on en a pas, il faut pisser -}
    | rcolor == atout && null atoutsPossibles = resteDesCartes
      
    -- Si on commence à l'atout, faut monter
    | rcolor == atout = atoutsPossibles

    -- On est à une couleur. Si on peut fournir, on fournit
    | not $ null cartesDeLaCouleur = cartesDeLaCouleur
    
-- On est à une couleur et on peut pas fournir

    -- On peut pas fournir mais le coequipier est maître : ou on surcoupe, on pisse
    | coequipierMaitreP = atoutsPossibles ++ resteDesCartes

    -- Si on n'a pas d'atout, on pisse
    | null atoutsPossibles = resteDesCartes

    
    -- TODO

    -- Sinon, on a de l'atout et on doit couper
     |otherwise = atoutsPossibles
    
  where atoutsJoues = [ci | ci <- plays, _cColor ci == atout]
        meilleurAtoutJoue = intToTrump $ maximum $ trumpToInt . _cRank <$> atoutsJoues
        cartesDeLaCouleur = [ci | ci <- cards, _cColor ci == rcolor]
        meilleurAtoutQue a = [ci | ci <- cards , _cColor ci == atout, trumpToInt (_cRank ci) > trumpToInt a]
        resteDesCartes = [ci | ci <- cards, _cColor ci /= rcolor, _cColor ci /= atout]
        atoutsPossibles
                | null atoutsJoues || null atoutsAuDessus = [ci | ci <- cards, _cColor ci == atout] 
                | otherwise = meilleurAtoutQue meilleurAtoutJoue

            where atoutsAuDessus = meilleurAtoutQue meilleurAtoutJoue

