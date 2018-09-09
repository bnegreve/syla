{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Coinche.Sequences.Core where
import Coinche.Sequences.Manche
import Coinche.Game
import Coinche.Rules
import Coinche.Types
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
import qualified Data.ByteString.Char8 as B

recruitTimer :: Int
recruitTimer = 30

announceTimer :: Int
announceTimer = 300

playingTimer :: Int
playingTimer = 300

data GameStatus = Recruiting | Announcing | Playing | StandBy
  deriving Eq
data CoincheGame = CoincheGame { _cgThreadID :: ThreadId,
                                 _cgAnnonceManager :: AnnonceManager,
                                 _cgGameManager :: GameManager,
                                 _cgPlayers :: A.Array Player T.Text,
                                 _cgNbPlayers :: Int,
                                 _cgStatus :: MVar GameStatus }

makeLenses ''CoincheGame
type CoincheGameT = StateT CoincheGame


initCoincheGame = do
        me <- myThreadId
        status <- newMVar StandBy
        pure $ CoincheGame me initAnnonceManager initGameManager initPlayerList 0 status






handler coincheChan = onPubmsg $ mainHandler coincheChan

mainHandler coincheChan src@(Channel chan nick) text
    | T.toLower chan /= T.toLower coincheChan = pure ()
    | otherwise = do
            statusM  <- use cgStatus
            status <- liftIO $ readMVar statusM
            case status of
              StandBy ->  recruitingHandler nick chan text
              Recruiting -> recruitingHandler nick chan text
              Announcing -> mainAnnounceHandler nick chan text
              Playing -> mainGameHandler nick chan text
--              otherwise -> undefined

recruitingHandler ::  T.Text -> T.Text -> T.Text -> IRC CoincheGame ()
recruitingHandler nick chan str 
    | not $ "!s"  `T.isPrefixOf` (T.toLower str)= pure ()
    | otherwise = do
            statusM <- use cgStatus
            status <- liftIO $ readMVar statusM

            -- Si on démarre une game, on change le status et on balance le thread
            when (status == StandBy) $ do
                    sendIntroMessage
                    liftIO $ swapMVar statusM Recruiting
                    cgNbPlayers .= 0
                    startTimer chan recruitTimer
                    sendMessage chan "Partie démarée"

            players <- use cgPlayers
            
            nbPlayers <- use cgNbPlayers
            
            -- On check si le joueur est déjà inscrit
            if not $ null [i | i <- [1..nbPlayers], players A.! (toEnum (i - 1)) == nick ] then do 
                    sendMessage chan $ T.concat ["Ce gros con de ", nick , " est déjà enregistré"]
             else do
            -- On ajoute le joueur et on modifie le compteur
                cgPlayers . ix (toEnum nbPlayers)  .= nick -- NORMAL car toEnum commence à partir de 0
                cgNbPlayers += 1
                sendMessage chan $ T.concat ["Player " , T.pack $ show $ 1+ nbPlayers, " " ,nick ,  " a été enregistré."]

                -- Si on est 4, on lance la sequence suivante
                when (nbPlayers + 1 == 4) $ launchAnnounceSequence chan
      where sendIntroMessage = sendMessage chan "On commence une partie, on recrute !"

launchAnnounceSequence :: T.Text -> IRC CoincheGame ()
launchAnnounceSequence chan = do
    -- On prévient qu'on démarre la séquence
    sendIntroMessage
    killTimer
    startTimer chan announceTimer

    players <- use cgPlayers
    sendMessage chan $ "Distribution des cartes. Il est temps d'annoncer bande de gros tas !"
    distribuerCartes players

    statusM <- use cgStatus

    cgAnnonceManager .= AnnonceManager Nothing (cycle $ A.elems players)
    liftIO $ swapMVar statusM Announcing

    pure ()

    -- DEBUG Stop timer
--    resetGame


 where sendIntroMessage = sendMessage chan "on commence !" 
       distribuerCartes players = do
               cards <- liftIO shuffleCards
               let distrib = A.listArray (P_1,P_4) $ Hand . sortCards <$>  group' 8 cards

               cgGameManager . gmGame . gPlayersHands .= distrib
               zipWithM sendHand (A.elems players) (A.elems distrib)   
               zipWithM sendHandPrivMsg (A.elems players) (A.elems distrib)   

mainAnnounceHandler :: T.Text -> T.Text -> T.Text -> IRC CoincheGame ()
mainAnnounceHandler nick chan str = do annonceManager <- use cgAnnonceManager
                                       let (ret,retState) = runState (announceHandler nick chan str) annonceManager
                                       cgAnnonceManager .= retState
                                       case ret of
                                         Discarded -> pure ()
                                         Finished -> launchGameSequence chan-- TODO
                                         Continuing -> do
                                                 killTimer
                                                 playersNames <- use cgPlayers
                                                 let nextPlayer = head $ _mPlayers retState
                                                 if (isJust $ _mBest retState) then do
                                                         let (bestAn,bestName) =  fromJust (_mBest retState)
                                                         sendMessage chan $ T.concat ["La meilleure annonce à présent est : ", showAnnonce bestAn, " par ", bestName, ". À ", nextPlayer, " de jouer." ]
                                                    else do 
                                                       sendMessage chan $ T.concat ["à ", head $ _mPlayers retState]
                                                 startTimer chan announceTimer
                                                 
                                         WrongEntry -> sendMessage chan $ T.concat ["Ou je n'ai pas compris ", str, ", ou c'est invalide !"]
launchGameSequence chan = do
        bestAnnonce <- use $ cgAnnonceManager . mBest
        if isNothing bestAnnonce then sendMessage chan "Personne n'a annoncé, on s'arrête !" >> resetGame
         else do

                killTimer
                sendMessage chan "C'est partiiiiiiiii"
                players <- use cgPlayers
                let (Annonce _ color) = fst $ fromJust bestAnnonce
                cgGameManager . gmPlayers .= players
                cgGameManager . gmAtout .= A color     
                cgGameManager. gmGame . gJoueursRestants .= range (P_1,P_4)

                statusM <- use cgStatus
                liftIO $ swapMVar statusM Playing
                startTimer chan playingTimer

                sendMessage chan $ T.concat ["Au joueur ", players A.! P_1, " de commencer."]



mainGameHandler nick chan str = do
       gameManager <- use cgGameManager
       annonceManager <- use cgAnnonceManager
       let (retValue, retState) = runState (gameHandler nick chan str) gameManager 
       cgGameManager .= retState
       case retValue of
        Discarded -> pure () 
        WrongEntry -> sendMessage chan $ T.concat ["Je n'ai pas compris :", str]
        Finished -> sendMessage chan "C'est fini !" >> onFinish chan >> resetGame
        Continuing -> do killTimer 
                         startTimer chan playingTimer 
                         let game = _gmGame retState
                             players = _gmPlayers retState
                             lastPli = head $ _gPlisJoues game
                             lastWinnerNum = snd $ head $ _gPlisJoues game
                             lastWinner = players A.! lastWinnerNum
                             pliCourant = game ^. gPliCourant 
                             playerCourant = head $ _gJoueursRestants game
                             playerCourantJeu = _gPlayersHands game A.! playerCourant
                             annonce = fst $ fromJust $ _mBest annonceManager

                             
                         if pliCourant ^. _w  == [] && (not $ null $ _gPlisJoues game) then do
                                    sendMessage chan $ T.concat ["Pli terminé. Le vainqueur est : ", lastWinner, ". À lui de commencer."]
                                    sendMessage chan $ T.concat ["Le pli était :", showPli players $ fst lastPli]
                                    zipWithM_ sendHand (A.elems $ players) (A.elems $ _gPlayersHands game)
                           else do sendMessage chan $ T.concat ["Le pli est : ", showPli players pliCourant,". À ", players A.! playerCourant, ". L'annonce est ", showAnnonce annonce]
                                   sendNotice (players A.! playerCourant) $ T.unwords $ showCard <$> (view  _w playerCourantJeu)

onFinish chan = do
        (Annonce _ atout) <- fst . fromJust <$> (use $ cgAnnonceManager . mBest)
        game <- use $ cgGameManager . gmGame
        (valeurs,_) <- runStateT (compterPlis $ A atout) game
        sendMessage chan $ T.pack $ show $ valeurs

    
sendToPlayer :: Player -> T.Text -> IRC CoincheGame ()
sendToPlayer player msg = do
         target <- use $ cgPlayers . ix player
         sendMessage target msg
sendHand player (Hand h)  = f player $ T.unwords $ showCard <$> h
  where f = sendNotice 
sendHandPrivMsg player (Hand h)  = f player $ T.unwords $ showCard <$> h
  where f = sendMessage
        --f =  sendMessage
showPli :: A.Array Player T.Text -> Pli -> T.Text
showPli players (Pli pli) = T.unwords $ fmap (\(card,player) -> T.concat $ [showCard card , "(", players A.! player , ")" ]) pli



group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = (take n l) : (group' n (drop n l))
    | otherwise = error "Negative n"

startTimer ::  T.Text -> Int -> IRC CoincheGame ()
startTimer chan time = do
    statusM <- use cgStatus
    state <- getIRCState
    timer <- liftIO $ forkIO $ do
                threadDelay $ time * 10^6
                status <- takeMVar statusM
                putMVar statusM StandBy
                runIRCAction (sendMessage chan "Partie terminée.") state 
    cgThreadID .= timer           

resetGame :: IRC CoincheGame () 
resetGame = do
        status <- use cgStatus
        liftIO $ do
                emptyP <- isEmptyMVar status
                if emptyP then putMVar status StandBy
                          else void $ swapMVar status StandBy
        killTimer

                
killTimer ::  IRC CoincheGame ()
killTimer= use cgThreadID >>= liftIO . killThread

