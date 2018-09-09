{-# LANGUAGE OverloadedStrings#-}
module Coinche.Parser where


import Coinche.Rules
import Coinche.Types
import qualified Data.Text as T
import Control.Monad
import Text.Read


readAnnonceVal :: T.Text -> Maybe Annonce_Val
readAnnonceVal "80" = pure CV_80
readAnnonceVal "90" = pure CV_90
readAnnonceVal "100" = pure CV_100
readAnnonceVal "110" = pure CV_110
readAnnonceVal "120" = pure CV_120
readAnnonceVal "130" = pure CV_130
readAnnonceVal "140" = pure CV_140
readAnnonceVal "150" = pure CV_150
readAnnonceVal "160" = pure CV_160
readAnnonceVal "170" = pure CV_170
readAnnonceVal "180" = pure CV_180
readAnnonceVal str  
    | 'c' == T.head str || 'C' == T.head str = pure CV_CAPOT
    | otherwise = Nothing

readColor :: T.Text -> Maybe Color
readColor x 
    | 'p' == T.head str = pure Spike
    | "tr" `T.isPrefixOf` str = pure Club
    |  "co" `T.isPrefixOf` str = pure Heart
    |  "ca" `T.isPrefixOf` str = pure Diamond
    | 's' == T.head str = pure SansAt
    | "ta" `T.isPrefixOf` str = pure ToutAt
    | "to" `T.isPrefixOf` str = pure ToutAt
    | otherwise = Nothing
   where str = T.toLower x


readRank :: T.Text -> Maybe Rank
readRank x 
    | 'a' == T.head str = pure C_As
    | 'k' == T.head str || 'r' == T.head str = pure C_K
    | 'q' == T.head str || 'd' == T.head str = pure C_Q
    | 'j' == T.head str || 'v' == T.head str = pure C_J
    | '7' == T.head str = pure C_7
    | '8' == T.head str = pure C_8
    | '9' == T.head str = pure C_9
    | "10" == str = pure C_10
    | otherwise = Nothing
  where str = T.toLower x

readAnnonce str = do

     if (length  wStr >= 2 ) then  do      
        val <- readAnnonceVal w1
        color <- readColor w2
        pure $ Annonce val color
      else Nothing  
 where  wStr = T.words str 
        (w1:w2:_) = wStr
readCard str = do
     if (length  wStr >= 2 ) then  do      
        val <- readRank w1
        color <- readColor w2
        pure $ Card val color
      else Nothing  
 where  wStr = T.words str 
        (w1:w2:_) = wStr

