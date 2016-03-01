--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


--------------------------------------------------------------------------------
import Data.Maybe
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
import Text.Show.Pretty (ppShow)

import Text.Megaparsec.Pos
import Data.Aeson.Encode.Pretty
import Data.Aeson
import TypeInfo as T
import LambdaCube.Compiler as C hiding (ppShow)

import Cache
import Hash

--------------------------------------------------------------------------------
--runApp x = WS.runWebSocketsSnapWith (WS.ConnectionOptions $ putStrLn "pong received") x
runApp x = WS.runWebSocketsSnap x

--app :: (String -> IO (Either String Pipeline, Infos)) -> Snap ()
app compiler ch = Snap.route
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
                    --print fname
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
              bs <- BC.unpack <$> WS.receiveData c
              --print bs
              let h = mkHash bs
              r <- lookupCache ch h
              json <- case r of
                Right add -> do
                  json <- catchErr er $ encodePretty . ff <$> compiler bs
                  add json
                  return json
                Left x -> return x
              WS.sendTextData c json
              go
        go
        putStrLn $ "compileApp ended"
      where
        toTypeInfo (s,e,m) = TypeInfo (cvtRange $ C.Range s e) m
        cvtRange (C.Range s e) = T.Range (sourceLine s) (sourceColumn s) (sourceLine e) (sourceColumn e)

        ff (Left err, (infos, _)) = CompileError (V.fromList eloc) err $ convertInfos infos
          where
            eloc = map cvtRange $ errorRange infos
        ff (Right ppl, (infos, dsg)) = Compiled dsg (prettyShowUnlines ppl) ppl $ convertInfos infos

        er e = return $ encodePretty $ CompileError mempty ("\n!FAIL\n" ++ e) mempty

        convertInfos is = toTypeInfo <$> V.fromList [ (a, b, unlines c) | (C.Range a b, c) <- listTypeInfos is ]

--------------------------------------------------------------------------------
main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- commandLineAppConfig Snap.defaultConfig
  compiler <- preCompile [] ["exercises"] WebGL1 "Prelude.lc"
  ch <- newCache 10
  Snap.httpServe config $ app compiler ch

