{-# LANGUAGE DeriveGeneric,TemplateHaskell,OverloadedStrings #-}
module Coinche.Engine.Types where 
import Data.List
import Control.Lens
import Data.Ix
import GHC.Generics
import qualified Data.Text  as T

data Player = P_1 | P_2 | P_3 | P_4
              deriving (Eq,Show,Enum, Ord, Ix)

data Annonce_Val = CV_80 | CV_90 | CV_100 | CV_110 | CV_120 | CV_130 | CV_140 | CV_150 | CV_160 | CV_170 | CV_180 | CV_CAPOT
                   deriving (Eq, Ord,Show)

data Annonce = Annonce { _aVal :: Annonce_Val, _aColor :: Color}
               deriving (Show, Eq)

data Rank = C_7 | C_8 | C_9 | C_J | C_Q | C_K | C_10 | C_As
            deriving (Show,Enum,Eq)
data Color = Spike | Heart | Diamond | Club | ToutAt | SansAt
             deriving (Eq,Enum, Show, Ord)
data Card = Card {_cRank :: Rank, _cColor :: Color}
            deriving (Show,Eq)
colors = [Spike,Heart,Diamond,Club]
newtype Atout = A Color
    deriving (Generic)
type Trump = Atout

instance Wrapped Atout

newtype RoundColor = RoundColor Color
    deriving (Generic, Show)
instance Wrapped RoundColor
_w
  :: (Functor f, Profunctor p, Wrapped s) =>
       p (Unwrapped s) (f (Unwrapped s)) -> p s (f s)
_w = _Wrapped'

makeLenses ''Annonce

showColor c = case c of 
                    Spike -> "Pi"
                    Heart -> "Co"
                    Diamond -> "Di"
                    Club -> "Tr"
                    -- ToutAt -> T.concat $ ["\xf0\x9f\x82\xab", red "\xf0\x9f\x82\xbb", "\xf0\x9f\x83\x9b"  ,red "\xf0\x9f\x83\x8b"]
                    ToutAt -> T.concat ["🂫",red "🂻", "🃛", red "🃋"]
--                    SansAt -> T.concat $ ["\xf0\x9f\x82\xa1",red "\xf0\x9f\x82\xb1", "\xf0\x9f\x83\x91", red "\xf0\x9f\x83\x81"]
                    SansAt -> T.concat ["\127137",red "🂱","🃑",red "\127169"]



red str = T.concat ["\ETX05",str, "\SI"]-- TODO

showRank :: Rank -> T.Text
showRank r = case r of
               C_As -> "As"
               C_K -> "R"
               C_Q -> "D"
               C_J -> "V"
               C_10 -> "10"
               otherwise -> T.pack $ show (fromEnum r + 7)

showCard (Card rank color) = T.concat $ [showRank rank, showColor color]

sortCards :: [Card] -> [Card]
sortCards l = concat $ sortSansAt <$> colors
  where colors = groupBy (\x y -> _cColor x == _cColor y) sortedByColor
        sortedByColor = sortBy (\x y -> _cColor x `compare` _cColor y) l
        sortSansAt = sortBy $ \x y -> fromEnum (_cRank x) `compare` fromEnum (_cRank y)


valeurRankNormal :: Rank -> Int
valeurRankNormal r = case r of
                      C_As -> 11 
                      C_10 -> 10 
                      C_K -> 4 
                      C_Q -> 3 
                      C_J -> 2 
                      otherwise -> 0

valeurRankAtout r = case r of
                      C_J -> 20
                      C_9 -> 14
                      otherwise -> valeurRankNormal r

valeurRankSansAT r = case r of
                      C_As -> 20
                      otherwise -> valeurRankNormal r 

valeurPli :: Atout -> [Card] -> Int
valeurPli (A atout) cards = sum $ fmap f cards
        where f (Card rank color) 
                | atout == color || atout == ToutAt = valeurRankAtout rank
                | atout == SansAt = valeurRankSansAT rank
                | otherwise = valeurRankNormal rank
            



{-TODO Mettre les codes utf8 des cartes cartes -}
