{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.List               (sort)
import qualified Data.Vector             as V
import           Control.Monad           (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Exception       (evaluate, ErrorCall, PatternMatchFail)
import           Control.DeepSeq
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified System.IO               as IO
import           System.Directory
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import qualified Snap.Core               as Snap
import qualified Snap.Http.Server        as Snap
import qualified Snap.Snaplet.Config     as Snap

import LambdaCube.Compiler.Pretty as C
import LambdaCube.Compiler as C

import TypeInfo as T
import Cache
import Hash

--------------------------------------------------------------------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- Snap.commandLineAppConfig Snap.defaultConfig
  compiler <- preCompile [] ["exercises"] WebGL1 "Prelude.lc"
  ch <- newCache 10
  Snap.httpServe config $ app compiler ch

--runApp x = WS.runWebSocketsSnapWith (WS.ConnectionOptions $ putStrLn "pong received") x
runApp x = WS.runWebSocketsSnap x

--app :: (String -> IO (Either String Pipeline, Infos)) -> Snap ()
app compiler ch = Snap.route
    [ (,) "compile"      $ runApp compileApp
    , (,) "exerciselist" $ runApp exerciselist
    , (,) "getexercise"  $ runApp getexercise
    ]
  where

    exerciselist :: WS.ServerApp
    exerciselist pending = do
        c <- WS.acceptRequest pending
        forever $ do
              (_ :: BC.ByteString) <- WS.receiveData c
              list <- sort . filter (`notElem` [".",".."]) <$> getDirectoryContents "exercises"
              print list
              WS.sendTextData c $ encodePretty $ toJSON list

    getexercise :: WS.ServerApp
    getexercise pending = do
        c <- WS.acceptRequest pending
        forever $ do
              name <- WS.receiveData c
              let fname = "exercises/" ++ BC.unpack name
              b <- doesFileExist fname
              when b $ do
                    src <- readFile fname
                    --print fname
                    WS.sendTextData c $ encodePretty $ toJSON src

    compileApp :: WS.ServerApp
    compileApp pending = do
        putStrLn "compileApp"
        c <- WS.acceptRequest pending
        --WS.forkPingThread c 5 
        forever $ do
              WS.sendPing c ("hello" :: B.ByteString)
              bs <- BC.unpack <$> WS.receiveData c
              --print bs
              let h = mkHash bs
              r <- lookupCache ch h
              json <- case r of
                Left json -> return json
                Right add -> do
                  json <- catchErr er $ encodePretty . ff <$> compiler bs
                  add json
                  return json
              WS.sendTextData c json
        putStrLn "compileApp ended"
      where
        cvtRange (C.Range _ (SPos r c) (SPos r' c')) = T.Range r c r' c'

        ff (Left err, (infos, _))    = CompileError (V.fromList $ cvtRange <$> errorRange infos) (plainShow err) $ convertInfos infos
        ff (Right ppl, (infos, dsg)) = Compiled dsg (prettyShowUnlines ppl) ppl $ convertInfos infos

        er e = return $ encodePretty $ CompileError mempty ("\n!FAIL\n" ++ e) mempty

        convertInfos is = V.fromList [TypeInfo (cvtRange r) $ C.plainShow $ C.vcat c | (r, c) <- listTypeInfos is ]

catchErr :: (MonadCatch m, NFData a, MonadIO m) => (String -> m a) -> m a -> m a
catchErr er m = (force <$> m >>= liftIO . evaluate) `catch` getErr `catch` getPMatchFail
  where
    getErr (e :: ErrorCall) = catchErr er $ er $ show e
    getPMatchFail (e :: PatternMatchFail) = catchErr er $ er $ show e

