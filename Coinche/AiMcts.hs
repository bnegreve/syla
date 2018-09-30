{-# LANGUAGE TemplateHaskell #-}

module Coinche.AiMcts where


import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad
import Control.Lens
import Coinche.Types
import Coinche.Game
import Coinche.Mcts
import System.Random.Shuffle
import System.Random
import Coinche.Rules
import Data.List
import qualified Data.Array as A
import System.Environment 
import qualified Data.Map as M
import Coinche.Ai

data MctsNode = MN { _mnGame :: Game,
                     _mnSumScore :: Double,
                     _mnNbSim :: Int,
                     _mnAsset :: Atout,
                     _mnCard :: Card, 
                     _mnChildren :: [ MctsNode ]
                   } 
  
makeLenses ''MctsNode

mctsSimul :: Atout -> Int -> Game -> Player -> [Card] -> IO [(Card, Double)]
mctsSimul trump nsim game player cards = do
  forM cards $ \c -> do
    root <-  mctsLoop (newNode (jouerCarte' trump game c) trump) nsim 0
--    putStrLn $ "" ++ show c ++ show (nodeScore root)
    pure (c, nodeScore root)

mcts :: Atout -> Int -> Int -> Game -> Player -> [Card] -> IO Card
mcts trump ngames nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> do
    remcardsshuffled <- shuffleM remcards
--    possiblegame <- samplePossibleGameSmart pogame player remcardsshuffled
    possiblegame <- samplePossibleGame pogame player remcards
    mctsSimul trump nsim possiblegame player legalcards
  let cardscores =
        (\x -> (fst $ head x, sum $ snd <$> x))
        <$> transpose allscores :: [(Card,Double)] in do
--    putStrLn ""
    pure $ bestmove cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player


      -- putStrLn $ (take depth (cycle "-")) ++ "calling mctsloop, depth " ++ (show depth) ++ " iter : " ++ (show n)
      --   ++ " score : " ++ (show (_mnSumScore node)) ++ " nsims " ++ (show (_mnNbSim node))
      -- putStrLn $ (take depth (cycle "-")) ++ "return from mctsloop, depth " ++ (show depth) ++ " iter : " ++ (show n)
      --   ++ " score : " ++ (show (_mnSumScore newnode)) ++ " nsims " ++ (show (_mnNbSim newnode))

mctsLoop :: MctsNode -> Int -> Int -> IO MctsNode
mctsLoop node nsim n 
  | n == 10 = pure node
  | otherwise = do
      newnode <- mctsRec node nsim 0
      mctsLoop newnode nsim (n+1) 
    
-- take node, runs n simulations and returns the same node with update stats
mctsRec :: MctsNode -> Int -> Int -> IO MctsNode
mctsRec node nsim depth
  | haschildren = do
      child' <- mctsRec child nsim (depth+1)
      pure $ updateNode node child child'
  | otherwise = do
          node <- mctsRollout node nsim
          pure $ expandNode node
  where haschildren = not $ null $ _mnChildren node
        child =  selectChild node


           
-- 
updateNode :: MctsNode -> MctsNode -> MctsNode -> MctsNode
updateNode node oldchild newchild =
  node { _mnSumScore = (_mnSumScore node) + scorediff,
         _mnNbSim = (_mnNbSim node) + nsimdiff }
  where scorediff = (_mnSumScore newchild) - (_mnSumScore oldchild)
        nsimdiff = (_mnNbSim newchild) - (_mnNbSim oldchild)


computeUCB :: MctsNode -> Int -> Double
computeUCB node totalnsim = 
  w / n + 1.44 * (sqrt (log nn) / n)
  where w = nodeScoreNormal node
        nn = fromIntegral totalnsim
        n = fromIntegral $ _mnNbSim node
  
-- Select a child from an expanded node
-- Fails if the node has not been expanded
selectChild :: MctsNode -> MctsNode
selectChild node = 
  fst $ head $ sortBy (\(a,b) (a',b') -> compare b b') scores
  where totalnsim = sum $ _mnNbSim <$> (_mnChildren node)
        scores = [ (child, (computeUCB node totalnsim))
                 | child <- _mnChildren node ] 

                                          
expandNode :: MctsNode -> MctsNode
expandNode node = node {
  _mnChildren = [ newNode (jouerCarte' trump game card) trump |
                  card <- coupsPossibles' trump game ] }
  where trump = _mnAsset node
        game = _mnGame node 

-- Runs a nbsim rollouts on a child-less node and returns a node with updated stats
mctsRollout :: MctsNode -> Int -> IO MctsNode
mctsRollout  node nbsim = do 
  scores <- forM [1..nbsim] $ \_ -> rollout player trump game
  pure $ node { _mnSumScore = sum scores,
                _mnNbSim = nbsim }
  where trump = _mnAsset node
        game = _mnGame node
        player = head $ _gJoueursRestants game

newNode :: Game -> Atout -> MctsNode
newNode game trump = MN game 0 0 trump []

nodeScore :: MctsNode -> Double
nodeScore node = (_mnSumScore node) / (fromIntegral (_mnNbSim node))

nodeScoreNormal :: MctsNode -> Double
nodeScoreNormal node =
  (nodeScore node) / 162.0

