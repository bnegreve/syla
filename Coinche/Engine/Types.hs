{-# LANGUAGE DeriveGeneric,TemplateHaskell,OverloadedStrings #-}
module Coinche.Engine.Types where 
import Data.List
import Control.Lens
import Data.Ix
import GHC.Generics
import qualified Data.Text  as T
import qualified Data.Array as A
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Binary

data Player = P_1 | P_2 | P_3 | P_4
              deriving (Eq,Show,Enum, Ord, Ix, Generic)

data BidVal = CV_80 | CV_90 | CV_100 | CV_110 | CV_120 | CV_130 | CV_140 | CV_150 | CV_160 | CV_170 | CV_180 | CV_CAPOT
                   deriving (Eq, Ord,Show)


data Rank = C_7 | C_8 | C_9 | C_J | C_Q | C_K | C_10 | C_As
            deriving (Show,Enum,Eq,Generic)
data Color = Spike | Heart | Diamond | Club | ToutAt | SansAt
             deriving (Eq,Enum, Show, Ord, Generic)
data Card = Card {_cRank :: Rank, _cColor :: Color}
            deriving (Show,Eq, Generic)

colors = [Spike,Heart,Diamond,Club]
newtype Atout = A Color
    deriving (Generic,Show,Eq)
type Trump = Atout

instance Wrapped Atout

data Bid = Bid { _bVal :: BidVal, _bColor :: Trump}
               deriving (Show, Eq)

instance Ord Bid where
  b `compare` b' = _bVal b `compare` _bVal b'

newtype RoundColor = RoundColor Color
    deriving (Generic, Show)
instance Wrapped RoundColor
_w
  :: (Functor f, Profunctor p, Wrapped s) =>
       p (Unwrapped s) (f (Unwrapped s)) -> p s (f s)
_w = _Wrapped'

makeLenses ''Bid

newtype Hand = Hand {_hand :: [Card]}                      
  deriving (Generic, Show)
instance  Wrapped Hand 


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
            deriving Generic

          
makeLenses ''Game

instance Binary Player
instance Binary Hand
instance Binary Pli
instance Binary Card
instance Binary Rank
instance Binary Color
instance Binary Game

type CardGameT  = ReaderT Bid
type CardGameTP = CardGameT Identity
type CardGameTIO = CardGameT IO

askTrump :: (Monad m) => CardGameT m Trump
askTrump = _bColor <$> ask

