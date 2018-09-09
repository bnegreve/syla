{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Coinche.Main where

import Coinche.Sequences.Manche
import Coinche.Sequences.Core
import Control.Lens

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

coincheChan = "#coinche"

--main = run "irc.wanascape.net" 6667 "curry"
main = run "irc.epiknet.org" 6667 "Bender"

run :: B.ByteString -> Int -> T.Text -> IO ()
run host port nick = do
        coinche <- initCoincheGame 
        let conn = plainConnection host port & logfunc .~ stdoutLogger
            cfg  = defaultInstanceConfig nick & handlers %~ ([handler coincheChan,handler "#stampede"] ++ )
            conn' = conn & flood .~ 0.1
        runClient conn' (cfg & channels .~ [coincheChan,"#stampede"]) coinche


