module Coinche.Mcts where

import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Types
import Coinche.Game
import Coinche.Rules
import Data.List
import qualified Data.Array as A

{- create a partition of l where each member of the partition is of size n-}
group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = (take n l) : (group' n (drop n l))
    | otherwise = error "Negative n"

terminated g =
  and $ null . view _w <$> A.elems (_gPlayersHands g) 
  
distribuerCartes = do
  cards <- shuffleCards
  pure $ A.listArray (P_1,P_4) $ Hand . sortCards <$>  group' 8 cards

gagnantPli :: Atout -> Pli -> Player
gagnantPli atout pli = winner
  where (card, winner) = foldl1 f (view _w pli) -- La carte initiale determine la couleur du pli
        f acc el = case comparerCarte atout (fst acc) (fst el) of
                     GT -> acc
                     LT -> el
                     EQ -> undefined
jouerCarte' :: Atout -> Game -> Card -> Game
jouerCarte' atout g c = gFinal
  where g' = g & gJoueursRestants %~ tail
        g'' = g' & gPliCourant . _w %~ (++ [(c,curPlayer)])
        g''' = g'' & gPlayersHands . ix curPlayer . _w %~ delete c
        gagnant = gagnantPli atout (_gPliCourant g''')
        gFinal = if null $ _gJoueursRestants g''' then
                 g'''{_gPlisJoues = _gPlisJoues g''' ++ [(_gPliCourant g''',gagnant)],
                     _gPliCourant = Pli [],
                     _gJoueursRestants = take 4 $ dropWhile (/= gagnant) $ cycle [P_1 .. P_4] }
               else
                 g'''
        curPlayer = head $ _gJoueursRestants g

coupsPossibles' :: Atout -> Game -> [Card]
coupsPossibles' atout g = coupsPossibles atout (RoundColor roundColor) False (fst <$> pliCourant) curHand
  where pliCourant = view _w $ _gPliCourant g
        curPlayer =  head $ _gJoueursRestants g
        curHand = _gPlayersHands g A.! curPlayer 
        roundColor = _cColor $ head $ fst <$> pliCourant        

score g = 100

rollout :: Atout -> Game -> Double
rollout atout g
  | terminated g = score g
  | otherwise = trace (show $ _gJoueursRestants g) $ 
      rollout atout $ jouerCarte' atout g (pickCard  coups)
      where pickCard = head
            coups = coupsPossibles' atout g

main = do
  hands <- distribuerCartes
  let g = initGame{_gJoueursRestants = [P_1, P_2, P_3, P_4], _gPlayersHands = hands}
      atout = A Heart
      val = rollout atout g
  print $ terminated g
  print val
  