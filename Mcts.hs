module Coinche.Mcts where

import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Types
import Coinche.Game
import System.Random.Shuffle
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

score :: Player -> Atout -> Game -> Int
score moi atout g
  | lastWinner == moi || lastWinner == coequipier moi = compteDesPoints + 10
  | otherwise = compteDesPoints
  where plisJoues = _gPlisJoues g
        valeurs = [(valeurPli atout (fmap fst pli), gagnant) | (Pli pli,gagnant) <- plisJoues] 
        compteDesPoints = foldl f 0 valeurs
        f acc (val, gagnant)
          | gagnant == moi || coequipier gagnant == moi = acc + val
          | otherwise = acc
        lastWinner = snd $ last plisJoues 

rollout :: Player -> Atout -> Game -> Double
rollout moi atout g
  | terminated g = fromIntegral $ score moi atout g
  | otherwise = rollout moi atout $ jouerCarte' atout g (pickCard  coups)
      where pickCard = head -- TODO
            coups = coupsPossibles' atout g

{- Evalue la valeur d'un état  -}
rollout' :: Player -> Atout -> Game -> Int -> Int -> IO (Card,Double)
rollout' me atout g n maxdepth
--  | terminated g = pure (score me atout g, g)
  | length coups == 1 = pure (head coups, rollout me atout g)
  | otherwise = do
      (g',remainingCards) <- gamePublicPlayer me g
      stats <- forM (coupsPossibles' atout g') $ \ci ->
        let g'' = jouerCarte' atout g' ci
	in do
          if maxdepth == 0 then
            do scores <- forM [1..n] $ \_ -> pure $ rollout me atout g''
               pure $ (ci, (sum scores) / fromIntegral n)
            else do
             results <- forM [1..n] $ \_ -> do newGame <- makePartialGame me g'' remainingCards
                                               rollout' me atout newGame n (maxdepth - 1)
	     pure $ (ci,sum (snd <$> results) / fromIntegral n)
      let bestMove = maximumBy (\(c,v) (c',v') -> v `compare` v') stats 
      pure $ bestMove    


      --coup <- head <$> shuffleM $ coupsPossibles' atout g'
      --rollout moi atout $ jouerCarte' atout g' (pickCard  coups)
      where 
            coups = coupsPossibles' atout g

test = do
  hands <- distribuerCartes
  let g = initGame{_gJoueursRestants = [P_1, P_2, P_3, P_4], _gPlayersHands = hands}
      atout = A Heart
      moi = P_2
  val <- rollout' moi atout g 1 0

--  putStrLn $ unlines $ show <$> plis
  return val

buildHands :: [Card] -> [(Player,Int)] -> [(Player,Hand)]
buildHands remainingCards sizes = fst $ foldr f ([],remainingCards) sizes
  where f (player,size) (hands,remCards) = let (hand, rest) = splitAt size remCards
                                  in ((player,Hand hand) :hands, rest)

-- affects random hands to other players. Never modifies the hand of player Me
makePartialGame :: Player -> Game -> [Card] -> IO Game
makePartialGame me g' remainingCards  = do
     randomizedRemainingCards <- shuffleM remainingCards
     let randomHands = buildHands randomizedRemainingCards $
                              [(pi,myHandSize) | pi <- remainingPlayers]
                              ++ [(pi,myHandSize - 1) | pi <- havePlayed]
 
     pure $ g' & gPlayersHands %~ (A.// randomHands)
  where myHandSize = length $ view _w $ _gPlayersHands g' A.! me
        remainingPlayers = _gJoueursRestants g' \\ [me]
        havePlayed =  (snd <$> (g' ^. gPliCourant . _w)) \\ [me]
        

        
{-
main n = do
   ret <- forM [1..n] (\ _ -> test)
   print $  (sum ret) / fromIntegral n
-}
