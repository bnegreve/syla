{-# LANGUAGE OverloadedStrings #-}
module Coinche.WebServer.Http where

import Control.Applicative
import Control.Monad.State
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Coinche.Engine
import Data.ByteString.Char8 
import qualified Data.Array as A

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

joinHandler :: Snap ()
joinHandler = do
    --name <- getParam "name"
    (bid, game) <- liftIO $ startCoinche
    let playerHand = (_gPlayersHands game) A.! P_2
    writeBS (pack $ show playerHand)
    -- maybe (writeBS "You must specify a valid name.")
    --      writeBS name

