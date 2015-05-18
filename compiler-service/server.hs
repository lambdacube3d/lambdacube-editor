--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO)
import           Control.Exception       (finally)
import           Control.Monad           (forever, unless)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import           Snap.Core               (Snap)
import qualified Snap.Core               as Snap
import qualified Snap.Http.Server        as Snap
import qualified Snap.Util.FileServe     as Snap
import qualified System.IO               as IO
import qualified System.Process          as Process

import Data.Aeson.Encode.Pretty
import IRJson
import Driver

--------------------------------------------------------------------------------
app :: Snap ()
app = Snap.route
    [ ("compile", compile)
    ]

--------------------------------------------------------------------------------
compile :: Snap ()
compile = do
    WS.runWebSocketsSnap $ compileApp

--------------------------------------------------------------------------------
compileApp :: WS.ServerApp
compileApp pending = do
    print "compileApp"
    c <- WS.acceptRequest pending
    let go = do
          bs <- WS.receiveData c
          B.writeFile "gfx.lc" bs
          res <- compileMain WebGL1 "." (ExpN "gfx")
          let json = encodePretty $ MyEither res
          WS.sendTextData c json
          go
    go
    putStrLn $ "compileApp ended"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  let config =
        Snap.setErrorLog  Snap.ConfigNoLog $
        Snap.setAccessLog Snap.ConfigNoLog $
        Snap.defaultConfig
  Snap.httpServe config app
