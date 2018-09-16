module Coinche.Mcts where

import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Types
import Coinche.Game
import System.Random.Shuffle
import System.Random
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
coupsPossibles' atout g = validMoves atout g curHand  --coupsPossibles atout (RoundColor roundColor) False (fst <$> pliCourant) curHand
  where --pliCourant = view _w $ _gPliCourant g
        curPlayer =  head $ _gJoueursRestants g
        curHand = _gPlayersHands g A.! curPlayer 
        --roundColor = _cColor $ head $ fst <$> pliCourant        
validMoves :: Atout -> Game -> Hand -> [Card]
validMoves atout g hand = coupsPossibles atout (RoundColor roundColor) False (fst <$> pliCourant) hand 
  where pliCourant = view _w $ _gPliCourant g
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

-- Plays a radom game and returns the score
rollout :: Player -> Atout -> Game -> IO Double
rollout me atout g
  | terminated g = pure $ fromIntegral $ score me atout g
  | otherwise = do
      let legalcards = coupsPossibles' atout g
      r <- getStdRandom (randomR (0, length legalcards - 1))
      rollout me atout $ jouerCarte' atout g (legalcards !! r)


-- {- Evalue la valeur d'un état  -}
-- rollout' :: Player -> Atout -> Game -> Int -> Int -> IO (Card,Double)
-- rollout' me atout g n maxdepth = do
-- --  | terminated g = pure (score me atout g, g)
--       (g',remainingCards) <- gamePublicPlayer me g
--       let possibleMoves
--             | me ==  curPlayer = coupsPossibles' atout g'
--             | otherwise = [head $ validMoves atout g' $ Hand remainingCards]
--       stats <- forM possibleMoves $ \ci ->
--         let g'' = jouerCarte' atout g' ci
--         in do
--           if maxdepth == 0 then
--             do 
--                scores <- forM [1..n] $ \_ -> do
--                  newGame <- makePartialGame me g'' $ if curPlayer == me then  remainingCards else tail remainingCards
--                  let x = rollout me atout newGame --g''
--                  print x
--                  pure x
--                pure $ (ci, (sum scores) / fromIntegral n)
--             else do
--              results <- forM [1..n] $ \_ -> do newGame <- makePartialGame me g'' remainingCards
--                                                rollout' me atout newGame n (maxdepth - 1)
--              pure $ (ci,sum (snd <$> results) / fromIntegral n)
--       let bestMove = maximumBy (\(c,v) (c',v') -> v `compare` v') stats 
--       trace (show stats) $ pure $ bestMove    


--       --coup <- head <$> shuffleM $ coupsPossibles' atout g'
--       --rollout moi atout $ jouerCarte' atout g' (pickCard  coups)
--       where 
--             coups = coupsPossibles' atout g
--             curPlayer = head $ g ^. gJoueursRestants
            
roll :: (s -> Bool) -> (s -> [c]) -> (s -> c -> s) ->  s -> IO s
roll terminalP coups jouer s 
  |terminalP s = pure s
  |otherwise = do let possibilites = coups s
                  randomCoup <- head <$> shuffleM possibilites
                  roll terminalP coups jouer (jouer s randomCoup) 
     
  
mcts :: (s -> Bool) -> (s -> [c]) -> (s -> c -> s) -> (s -> Double) -> Int -> s -> IO (c,Double)
mcts terminalP coups jouer eval maxdepth s
  |maxdepth == 1 = do tests <- forM (coups s) $ \ci -> do
                                val <- eval <$> roll terminalP coups jouer s
                                pure (ci,val)
                      pure $ maximumBy (\(c,v) (c',v') -> v `compare` v') tests
  |otherwise = do
               tests <- forM (coups s) $ \ci -> do
                                 val <- snd <$> mcts terminalP coups jouer eval (maxdepth - 1) s
                                 pure (ci,val)

               pure $ maximumBy (\(c,v) (c',v') -> v `compare` v') tests
     
  


-- test = do
--   hands <- distribuerCartes
--   let g = initGame{_gJoueursRestants = [P_1, P_2, P_3, P_4], _gPlayersHands = hands}
--       atout = A Heart
--       moi = P_2
--   val <- rollout' moi atout g 1 0

-- --  putStrLn $ unlines $ show <$> plis
--   print val
--   return val

-- Note: horrible function written by Sat


{-
main n = do
   ret <- forM [1..n] (\ _ -> test)
   print $  (sum ret) / fromIntegral n
-}
