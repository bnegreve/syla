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
                     _mnPlayer :: Player, 
                     _mnSumScore :: Double,
                     _mnNbSim :: Int,
                     _mnAsset :: Atout,
                     _mnCard :: Card, 
                     _mnChildren :: [ MctsNode ]
                   }

data MctsOpts = MO { _moNGames :: Int,
                     _moNLoops :: Int, 
                     _moNSim :: Int,
                     _moAlpha :: Double}
                     

makeLenses ''MctsNode

mctsAi :: MctsOpts -> Game -> Player -> Atout -> [Card] -> IO Card
mctsAi (MO ngames nloops nsim alpha) game player trump legalcards =
  mctsAi' trump ngames nloops nsim game player legalcards

mctsAi' :: Atout -> Int -> Int -> Int -> Game -> Player -> [Card] -> IO Card
mctsAi' trump ngames nloops nsim game player legalcards = do
  -- compute a score for each card in several possible games
  remcards <- remainingCards game player
  allscores <- forM [1..ngames] $ \_ -> do
    remcardsshuffled <- shuffleM remcards
--    possiblegame <- samplePossibleGameSmart pogame player remcardsshuffled
    possiblegame <- samplePossibleGame pogame player remcards
    mcts possiblegame player trump nloops nsim
    
  let cardscores =
        (\x -> (fst $ head x, sum $ snd <$> x))
        <$> transpose allscores :: [(Card,Double)] in do    
    -- putStrLn "MOVE "
    -- putStrLn $ show cardscores
    -- putStrLn $ show $ bestmove cardscores
    pure $ bestmove cardscores
  where
    pogame = partiallyObservedGame player game -- partially observed game from player

showTree :: MctsNode -> MctsNode -> Int -> IO ()
showTree node parent depth 
  | null children = do showNode node parent depth
  | otherwise = do
      showNode node parent depth 
      forM children (\c -> showTree c node (depth+1))
      pure ()
  where children = (_mnChildren node)

showNode :: MctsNode -> MctsNode -> Int -> IO ()
showNode node parent depth = do
  putStrLn (prefix ++ "SCORE " ++ (show (nodeScore node))
                   ++ " NSIMS " ++ (show (_mnNbSim node))
                   ++ " UCB " ++ (show (computeUCB node nn))
                   ++ " " ++ (show (_mnCard node)))
  where prefix = take (2*depth) $ cycle " "
        nn = _mnNbSim parent
  
mcts :: Game -> Player -> Atout -> Int -> Int -> IO [(Card, Double)]
mcts game player trump nloops nsim = do
  root' <- mctsLoop root nloops nsim
  -- putStrLn "End of MCTS"
  -- -- putStrLn (show (cardScores root'))
  -- showTree root' root' 0
  pure $ cardScores root'
  where root = expandNode $
               newNode game player trump (Card C_7 Club) -- dummy card for rootnode

-- calls mcts until we run out of budget (nloops)
mctsLoop :: MctsNode -> Int -> Int -> IO MctsNode
mctsLoop node nloops nsim
  | nloops == 0 = pure node
  | otherwise = do
      newnode <- mctsRec node nsim 0
      -- putStrLn ("End of Iter " ++ show (nloops))
      -- showTree newnode newnode 0
      -- putStrLn "-------------------"
      mctsLoop newnode (nloops-1) nsim
    
-- moves down the MCTS tree, select a new node, run rollouts for this
-- node, update the parent nodes 
mctsRec :: MctsNode -> Int -> Int -> IO MctsNode
mctsRec node nsim depth
  | haschildren = do
      thechild' <- mctsRec thechild nsim (depth+1)
      pure $ updateNode node thechild thechild' otherchildren
  | otherwise = do
          node <- mctsRollout node nsim
          pure $ expandNode node
  where haschildren = not $ null $ _mnChildren node
        thechild:otherchildren = orderChildrenUCB node

-- update statistics for a node
-- node: the node to update
-- oldchild: the child node that was selected, as it was before running the rollouts
-- newchild: the child node, after the rollouts
-- children: list of nodes that were not selected for rollouts
updateNode :: MctsNode -> MctsNode -> MctsNode -> [MctsNode] -> MctsNode
updateNode node oldchild newchild otherchildren =
  node { _mnSumScore = (_mnSumScore node) + scorediff,
         _mnNbSim = (_mnNbSim node) + nsimdiff,
         _mnChildren = newchild:otherchildren
       }
  where scorediff = (_mnSumScore newchild) - (_mnSumScore oldchild)
        nsimdiff = (_mnNbSim newchild) - (_mnNbSim oldchild)


computeUCB :: MctsNode -> Int -> Double
computeUCB node totalnsim
  | n == 0 = 1000.0 -- return a large value if we have not tried this move so far
  | otherwise = w / n + 0.08 * (sqrt (log nn) / n)
  where w = nodeScoreNormal node
        nn = fromIntegral totalnsim
        n = fromIntegral $ _mnNbSim node

-- returns the list of the children of a node sorted according to the UCB bound
-- TODO: inefficient (recompute UCB), I should improve it.
orderChildrenUCB :: MctsNode -> [MctsNode]
orderChildrenUCB parent =
  sortBy (\a b -> rcompare (computeUCB a totsim) (computeUCB b totsim)) children
  where children = _mnChildren parent
        totsim = sum $ _mnNbSim <$> children
        rcompare = flip compare

-- Select a child from an expanded node
-- Fails if the node has not been expanded
selectChild :: MctsNode -> MctsNode
selectChild node = 
  fst $ head $ sortBy (\(a,b) (a',b') -> compare b b') scores
  where totalnsim = _mnNbSim node -- sum $ _mnNbSim <$> (_mnChildren node)
        scores = [ (child, (computeUCB node totalnsim))
                 | child <- _mnChildren node ] 

-- Compte the direct children of a node and return a new updated node
expandNode :: MctsNode -> MctsNode
expandNode node = node {
  _mnChildren = [ newNode (jouerCarte' trump game card) player trump card |
                  card <- coupsPossibles' trump game ] }
  where trump = _mnAsset node
        game = _mnGame node
        player = _mnPlayer node

-- Runs a nbsim rollouts on a child-less node and returns a node with updated stats
mctsRollout :: MctsNode -> Int -> IO MctsNode
mctsRollout node nbsim = do 
  scores <- forM [1..nbsim] $ \_ -> rollout player trump game
  --  putStrLn $ "rollout -> " ++ show ((sum scores)/ (fromIntegral nbsim))
  pure $ node { _mnSumScore = sum scores,
                _mnNbSim = nbsim }
  where trump = _mnAsset node
        game = _mnGame node
        player = _mnPlayer node 
--        player = head $ _gJoueursRestants game
          

newNode :: Game -> Player -> Atout -> Card -> MctsNode
newNode game player trump card = MN game player 0 0 trump card []

nodeScore :: MctsNode -> Double
nodeScore node = (_mnSumScore node) / (fromIntegral (_mnNbSim node))

nodeScoreNormal :: MctsNode -> Double
nodeScoreNormal node =
  (nodeScore node) / 162.0

bestCard :: MctsNode -> Card
bestCard node =
  let best = foldl (\a b -> if (nodeScore a) > (nodeScore b)
                     then a else b) first rest in 
    (_mnCard best)
  where first:rest = (_mnChildren node)

cardScores :: MctsNode -> [(Card, Double)]
cardScores node =
  [ ( (_mnCard node), (nodeScore node) ) |
    node <- _mnChildren node,
    not $ isnan $ nodeScore node ]
  where isnan = \x -> x /= x
