{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module Coinche.WebServer.Http where

import Control.Applicative
import Control.Monad.State
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Coinche.Engine
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import qualified Data.Array as A
import Data.Aeson
import Data.Text

data Person = Person {
      name :: String
    , age  :: Int
    } deriving (Generic, Show)



main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("test/:echoparam", echoHandler)
          , ("join/:name", joinHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

instance ToJSON Hand where
  toJSON (Hand hand) = object [pack "hand" .= hand]
  
instance ToJSON Card where
  toJSON (Card rank color) = object [ pack "rank" .= (pack $ show rank)
                                    , pack "color" .= (pack $ show color) ]


joinHandler :: Snap ()
joinHandler = do
    --name <- getParam "name"
    (bid, game) <- liftIO $ startCoinche
    let playerHand = (_gPlayersHands game) A.! P_2
        response = encode playerHand
    writeBS $ BS.toStrict $ response 
    -- maybe (writeBS "You must specify a valid name.")
    --      writeBS name

