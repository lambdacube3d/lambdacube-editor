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

import qualified Data.Vector as V
import Data.List (sort)
import Snap.Snaplet.Config
import System.IO
import System.Directory
--import Control.Exception
--import Control.Monad.IO.Class
--import Control.DeepSeq

import Text.Parsec.Pos
import Data.Aeson.Encode.Pretty
import Data.Aeson
import TypeInfo
import LambdaCube.Compiler.Driver
import LambdaCube.Compiler.CGExp

--------------------------------------------------------------------------------
--runApp x = WS.runWebSocketsSnapWith (WS.ConnectionOptions $ putStrLn "pong received") x
runApp x = WS.runWebSocketsSnap x

app :: (String -> IO (Err (Pipeline, Infos))) -> Snap ()
app compiler = Snap.route
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
              list <- sort . filter (`notElem` [".",".."]) <$> getDirectoryContents "exercises"
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
        --WS.forkPingThread c 5 
        let go = do
              WS.sendPing c ("hello" :: B.ByteString)
              bs <- WS.receiveData c
              json <- catchErr er $ encodePretty . ff <$> compiler (BC.unpack bs)
              WS.sendTextData c json -- $ deepseq json json
              go
        go
        putStrLn $ "compileApp ended"

    toTypeInfo (s,e,m) = TypeInfo
      { startLine   = sourceLine s
      , startColumn = sourceColumn s
      , endLine     = sourceLine e
      , endColumn   = sourceColumn e
      , text        = m
      }

    ff (Left (ErrorMsg err), infos) = MyLeft (TypeInfo 0 0 0 0 err) $ toTypeInfo <$> V.fromList infos
    ff (Right (m, _), infos) = MyRight m $ toTypeInfo <$> V.fromList infos

    er e = return $ encodePretty $ MyLeft (TypeInfo 0 0 0 0 ("\n!FAIL err\n" ++ e :: String)) mempty

--------------------------------------------------------------------------------
main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- commandLineAppConfig Snap.defaultConfig
  compiler <- preCompile ["."] WebGL1 "DemoUtils"
  Snap.httpServe config $ app compiler
