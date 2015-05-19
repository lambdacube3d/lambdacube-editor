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

import Snap.Snaplet.Config
import System.IO
import Control.Exception
import Control.Monad.IO.Class
import Control.DeepSeq

import Data.Aeson.Encode.Pretty
import IRJson
import Driver
import Type (PolyEnv)

--------------------------------------------------------------------------------
app :: PolyEnv -> Snap ()
app prelude = Snap.route
    [ ("compile", compile)
    ]
  where
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
              json <- liftIO $ catchErr $ encodePretty . MyEither . either (Left . showErr) Right <$> compileMain' prelude WebGL1 (BC.unpack bs)
              WS.sendTextData c json
              go
        go
        putStrLn $ "compileApp ended"

    catchErr m = flip catch getErr $ do
        x <- m
        deepseq x `seq` return x
    getErr :: ErrorCall -> IO BL.ByteString
    getErr e = catchErr $ return $ encodePretty . MyEither $ Left (dummyPos, dummyPos, "\n!FAIL err\n" ++ show e)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- commandLineAppConfig Snap.defaultConfig
  Right prelude <- runMM (ioFetch ["."]) $ loadModule (ExpN "Prelude")
  Snap.httpServe config $ app prelude
