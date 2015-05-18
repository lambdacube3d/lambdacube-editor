module Main (main,run) where

import Debug.Trace

import Control.Bind
import Control.Timer (timeout,clearTimeout)
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Date
import Data.Time
import Data.Foldable (for_, foldl)

import Ace
import Ace.Types
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session
import qualified Ace.Range as Range

import qualified Graphics.WebGL as GL
import qualified Control.Monad.JQuery as J

import WebSocket
import DefaultText
import Data.Either
import Sample

import Data.Maybe
import Data.StrMap
import Data.Tuple
import Data.Array(filter,head)

import Backend
import IR
import Mesh
import Type
import Input

import PipelineJsonDecode
import qualified Data.Argonaut as A

import Data.Matrix (Mat(..))
import Data.Matrix4
import Data.Vector3

main = return unit

{-
  control-b - compile/build
  control-n - new
-}
foreign import addCommand """
  function addCommand(editor) {
    return function(cmdName) {
      return function(winKey) {
        return function(macKey) {
          return function(cmd) {
            return function() {
              editor.commands.addCommand({
                name: cmdName,
                bindKey: {win: winKey,  mac: macKey},
                exec: function(editor) {
                  cmd(editor)();
                }
              });
            };
          };
        };
      };
    };
  }
""" :: forall eff . Editor -> String -> String -> String -> (Editor -> Eff (ace :: EAce | eff) Unit) -> Eff (ace :: EAce | eff) Unit

foreign import tokenTooltip """
  function tokenTooltip(editor) {
    return function(getTy) {
      return function() {
          editor.tokenTooltip = new myTokenTooltip(editor,getTy);
      };
    };
  }
""" :: forall eff . Editor -> (Int -> Int -> Eff (ace :: EAce | eff) TypeInfoRecord) -> Eff (ace :: EAce | eff) Unit

run = GL.runWebGL "glcanvas" (\s -> trace s) $ \context -> do
  -- setup pipeline input
  let inputSchema = 
        { slots : fromList [ Tuple "stream"  {primitive: Triangles, attributes: fromList [Tuple "position"  TV3F, Tuple "normal" TV3F, Tuple "UVTex" TV2F]}
                           , Tuple "stream4" {primitive: Triangles, attributes: fromList [Tuple "position4" TV4F, Tuple "vertexUV" TV2F]}
                           ]
        , uniforms : fromList [Tuple "MVP" M44F, Tuple "MVP2" M44F]
        }
  pplInput <- mkWebGLPipelineInput inputSchema
  let --mvp = V4 (V4 0.4692207 (-0.28573585) (-0.9593549) (-0.9574381)) (V4 0.0 2.395976 (-0.122928835) (-0.12268323)) (V4 (-1.719497) (-7.797232e-2) (-0.2617912) (-0.26126814)) (V4 0.0 0.0 3.8834958 4.0755367)
      toLCMat4 :: Mat4 -> M44F
      toLCMat4 (Mat [x11, x21, x31, x41, x12, x22, x32, x42, x13, x23, x33, x43, x14, x24, x34, x44]) = let
          v1 = V4 x11 x21 x31 x41
          v2 = V4 x12 x22 x32 x42
          v3 = V4 x13 x23 x33 x43
          v4 = V4 x14 x24 x34 x44
        in V4 v1 v2 v3 v4

      updateInput t = do
        let pi = 3.141592653589793
            w  = 800
            h  = 600
            angle = pi / 24 * t
            cm = makeLookAt (vec3 3 1.3 0.3) (vec3 0 0 0) (vec3 0 1 0)
            mm = makeRotate angle (vec3 0 1 0)
            pm = makePerspective 45 (w/h) 0.1 100
            mvp = pm `mul` cm `mul` mm
        uniformM44F "MVP" pplInput.uniformSetter $ toLCMat4 mvp

  gpuCube <- compileMesh myCube

  addMesh pplInput "stream4" gpuCube []

  -- setup ace editor
  editor <- Ace.edit "editor" ace
  session <- Editor.getSession editor
  Editor.setTheme "ace/theme/terminal" editor
  Session.setMode "ace/mode/haskell" session
  Session.setValue defaultSrc session
  range <- Range.create 8 0 9 0
  Session.addMarker range "lc_error" "line" false session
  typeInfoRef <- newRef []
  let lessEqPos l c l' c' = l < l' || l == l' && c <= c'
  let lessPos l c l' c' = l < l' || l == l' && c < c'
  let getTypeInfo l c = do
        x <- readRef typeInfoRef
        let ps = flip filter x $ \(TypeInfo i) -> lessEqPos i.startLine i.startColumn l c && lessPos l c i.endLine i.endColumn
        let f Nothing (TypeInfo i) = Just i
            f (Just d) (TypeInfo i)
                | lessPos d.startLine d.startColumn i.startLine i.startColumn = Just i
                | otherwise = Just d
        return $ case foldl f Nothing ps of
          Nothing ->
              { startLine   : 0
              , startColumn : 0
              , endLine     : 0
              , endColumn   : 0
              , text        : ""
              }
          Just a -> a
  tokenTooltip editor getTypeInfo

  b <- J.body
  ui <- J.find "ui" b
  btnCompile <- J.create "<button>"
  J.setText "Compile" btnCompile
  btnCompile `J.append` b

  pipelineRef <- newRef Nothing
  let compile s = do
        trace "compile"
        src <- Session.getValue session
        send s src

      render ir = do
        trace "WebGL ready"
        ppl <- allocPipeline ir -- gfx03Pipeline -- samplePipeline
        trace "Pipeline allocated"

        setPipelineInput ppl (Just pplInput)
        sortSlotObjects pplInput
        trace "Setup pipeline input"

        old <- readRef pipelineRef
        case old of
          Nothing -> return unit
          Just p -> do
            disposePipeline p
            trace "Pipeline disposed"
        writeRef pipelineRef $ Just ppl
        trace "WebGL completed"

  socket <- webSocket "ws://localhost:8000/compile" $
    { onOpen    : \s -> do
        trace "socket is ready"
        compile s
        timerRef <- newRef Nothing
        Session.onChange session $ do
          t <- readRef timerRef
          case t of
            Nothing -> return unit
            Just a -> clearTimeout a
          writeRef timerRef =<< Just <$> timeout 1000 (compile s)

    , onClose   : trace "socket is closed"
    , onMessage : \s m -> do
        case A.jsonParser m >>= A.decodeJson of
          Left e -> trace $ "decode error: " ++ e
          Right (MyLeft e) -> trace $ "compile error: " ++ e
          Right (MyRight p infos) -> do
            render p
            writeRef typeInfoRef infos
    , onError   : \s m -> trace m
    }
  case socket of
    Left m -> trace $ "error: " ++ m
    Right ws -> do
      flip (J.on "click") btnCompile $ \_ _ -> do
        compile ws
        trace "clicked compile"
      addCommand editor "Compile" "Ctrl-B" "Command-B" (\_ -> compile ws)
      let renderLoop = do
            Milliseconds t <- nowEpochMilliseconds
            updateInput (t / 1000)
            mppl <- readRef pipelineRef
            case mppl of
              Nothing -> return unit
              Just ppl -> renderPipeline ppl
            timeout (1000/25) renderLoop
      -- render loop
      renderLoop
      return unit
