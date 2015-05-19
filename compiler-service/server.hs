--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import System.Directory
import Control.Exception
import Control.Monad.IO.Class
import Control.DeepSeq

import Data.Aeson.Encode.Pretty
import Data.Aeson
import IRJson
import Driver
import Type (PolyEnv)

--------------------------------------------------------------------------------
runApp x = WS.runWebSocketsSnapWith (WS.ConnectionOptions $ putStrLn "pong received") x
app :: PolyEnv -> Snap ()
app prelude = Snap.route
    [ (,) "compile" $ runApp compileApp
    , (,) "exerciselist" $ runApp exerciselist
    , (,) "getexercise" $ runApp getexercise
    ]
  where

    exerciselist :: WS.ServerApp
    exerciselist pending = do
        c <- WS.acceptRequest pending
        let go = do
              (_ :: BC.ByteString) <- WS.receiveData c
              list <- filter (`notElem` [".",".."]) <$> getDirectoryContents "exercises"
              print list
              WS.sendTextData c $ encodePretty $ toJSON list
              go
        go

    getexercise :: WS.ServerApp
    getexercise pending = do
        c <- WS.acceptRequest pending
        let go = do
              (name :: BC.ByteString) <- WS.receiveData c
              let fname = "exercises/" ++ BC.unpack name
              b <- doesFileExist fname
              if b then do
                    src <- readFile fname
                    print fname
                    WS.sendTextData c $ encodePretty $ toJSON src
                else return ()
              go
        go

    compileApp :: WS.ServerApp
    compileApp pending = do
        print "compileApp"
        c <- WS.acceptRequest pending
        WS.forkPingThread c 5 
        let go = do
              WS.sendPing c ("hello" :: B.ByteString)
              putStr "wait "
              bs <- WS.receiveData c
              putStr "recived "
              json <- liftIO $ catchErr $ encodePretty . MyEither . ff <$> compileMain' freshTypeVars prelude WebGL1 (BC.unpack bs)
              putStr "compiled "
              WS.sendTextData c $ deepseq json json
              putStrLn "sent"
              go
        go
        putStrLn $ "compileApp ended"

    ff (Left err, infos) = Left (showErr err, infos)
    ff (Right (m, _), infos) = Right (m, infos)

    catchErr m = flip catch getErr $ do
        x <- m
        evaluate $ deepseq x x
        --deepseq x `seq` return $ deepseq x x
    getErr :: ErrorCall -> IO BL.ByteString
    getErr e = catchErr $ return $ encodePretty . MyEither $ Left ((dummyPos, dummyPos, "\n!FAIL err\n" ++ show e), [])

--------------------------------------------------------------------------------
main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- commandLineAppConfig Snap.defaultConfig
  (Right prelude, _) <- runMM (map ("tt" ++) $ map show [0..]) (ioFetch ["."]) $ loadModule (ExpN "Prelude")
  Snap.httpServe config $ app prelude
