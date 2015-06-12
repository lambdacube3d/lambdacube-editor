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

import Data.Function
import Data.Maybe
import Data.StrMap
import Data.Tuple
import Data.Array(filter,head)

import Backend
import IR
import Mesh
import Type
import Input

import MeshJsonDecode
import PipelineJsonDecode
import qualified Data.Argonaut as A
import qualified Data.Argonaut.Core as AC

import Data.Matrix (Mat(..))
import Data.Matrix4
import Data.Vector3

import qualified DOM as DOM

main = return unit

foreign import getMousePos """
  function getMousePos(e) {
      return function() {
        var mouseX, mouseY;

        if(e.offsetX) {
            mouseX = e.offsetX;
            mouseY = e.offsetY;
        }
        else if(e.layerX) {
            mouseX = e.layerX;
            mouseY = e.layerY;
        }
        return [mouseX,mouseY];
      };
  }
""" :: forall eff a. J.JQueryEvent -> Eff (dom :: DOM.DOM | eff) [Number]

foreign import getJSON """
  function getJSON(uri) {
    return function(act) {
      return function() {
        $.getJSON(uri, function(data) {
          act(data)();
        });
      };
    };
  }
""" :: forall eff a. String -> (AC.Json -> Eff (dom :: DOM.DOM | eff) a) -> Eff (dom :: DOM.DOM | eff) Unit

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

foreign import add
  """
  function add(ob1) {
    return function(ob) {
      return function () {
        return ob.add(ob1);
      };
    };
  }
  """ :: forall eff. J.JQuery -> J.JQuery -> Eff (dom :: DOM.DOM | eff) J.JQuery

foreign import addMarkerImpl
  "function addMarkerImpl(range, clazz, type, inFront, self) {\
  \  return function() {\
  \    return self.addMarker(range, clazz, type, inFront);\
  \  };\
  \}" :: forall eff. Fn5 Range String String Boolean EditSession (Eff (ace :: EAce | eff) Int)

addMarker :: forall eff. Range -> String -> String -> Boolean -> EditSession -> Eff (ace :: EAce | eff) Int
addMarker range clazz _type inFront self = runFn5 addMarkerImpl range clazz _type inFront self

run = GL.runWebGL "glcanvas" (\s -> trace s) $ \context -> do
  Milliseconds baseTime <- nowEpochMilliseconds
  -- setup pipeline input
  let inputSchema = 
        { slots : fromList [ Tuple "stream"  {primitive: Triangles, attributes: fromList [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                           , Tuple "stream4" {primitive: Triangles, attributes: fromList [Tuple "position4" TV4F, Tuple "vertexUV" TV2F]}
                           , Tuple "line"    {primitive: Triangles, attributes: fromList [Tuple "position" TV3F]}
                           , Tuple "grid"    {primitive: Triangles, attributes: fromList [Tuple "position" TV3F]}
                           , Tuple "grid3d"  {primitive: Points,    attributes: fromList [Tuple "position" TV3F]}
                           , Tuple "quad"    {primitive: Triangles, attributes: fromList [Tuple "position" TV2F]}
                           , Tuple "cube"    {primitive: Triangles, attributes: fromList [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                           ]
        , uniforms : fromList [Tuple "MVP" M44F, Tuple "MVP2" M44F, Tuple "Time" Float, Tuple "Mouse" V2F]
        }
  pplInput <- mkWebGLPipelineInput inputSchema
  let toLCMat4 :: Mat4 -> M44F
      toLCMat4 (Mat [x11, x21, x31, x41, x12, x22, x32, x42, x13, x23, x33, x43, x14, x24, x34, x44]) = let
          v1 = V4 x11 x21 x31 x41
          v2 = V4 x12 x22 x32 x42
          v3 = V4 x13 x23 x33 x43
          v4 = V4 x14 x24 x34 x44
        in V4 v1 v2 v3 v4

      updateInput t = do
        w <- GL.getCanvasWidth context
        h <- GL.getCanvasHeight context
        setScreenSize pplInput (V2 w h)
        let pi = 3.141592653589793
            angle = pi / 24 * t
            cm = makeLookAt (vec3 3 1.3 0.3) (vec3 0 0 0) (vec3 0 1 0)
            mm = makeRotate angle (vec3 0 1 0)
            pm = makePerspective 30 (w/h) 0.1 100
            mvp = pm `mul` cm `mul` mm
            cm' = makeLookAt (vec3 4 0.5 (-0.6)) (vec3 0 0 0) (vec3 0 1 0)
            mvp2 = pm `mul` cm' `mul` mm

        uniformFloat "Time" pplInput.uniformSetter t
        uniformM44F "MVP" pplInput.uniformSetter $ toLCMat4 mvp
        uniformM44F "MVP2" pplInput.uniformSetter $ toLCMat4 mvp2

  gpuCube <- compileMesh myCube
  addMesh pplInput "stream4" gpuCube []

  gpuQuad <- compileMesh myQuad
  addMesh pplInput "quad" gpuQuad []

  let addRemoteModel sname uri = getJSON uri $ \m -> do
        case A.decodeJson m of
          Left e -> trace $ "decode error: " ++ e
          Right (MeshData mesh) -> do
            gpuMesh <- compileMesh mesh
            addMesh pplInput sname gpuMesh []
            sortSlotObjects pplInput
            return unit

  --addRemoteModel "grid3d" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/grid3d.mesh.json"
  --addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/logo2.mesh.json"
  --addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/logo3.mesh.json"
{-
  addRemoteModel "quad"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/quad.mesh.json"
  addRemoteModel "grid"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/grid.mesh.json"
  addRemoteModel "line"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/line.mesh.json"
  addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/ico.mesh.json"
-}
  -- setup ace editor
  editor <- Ace.edit "editor" ace
  session <- Editor.getSession editor
  Editor.setTheme "ace/theme/terminal" editor
  Session.setMode "ace/mode/haskell" session
  Session.setValue defaultSrc session
  typeInfoRef <- newRef []
  markerRef <- newRef []
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
  messagepanel <- J.find "#messagepanel" b
  statuspanel <- J.find "#statuspanel" b
  btnCompile <- J.find "#compilebutton" b
  exerciseselect <- J.find "#exerciseselect" b
  glcanvas <- J.find "#glcanvas" b
  flip (J.on "mousemove") glcanvas $ \e _ -> do
    w <- GL.getCanvasWidth context
    h <- GL.getCanvasHeight context
    [x,y] <- getMousePos e
    uniformV2F "Mouse" pplInput.uniformSetter $ V2 (x/w) (y/h)

  pipelineRef <- newRef Nothing
  let compile s = do
        trace "compile"
        J.setText "Compiling..." statuspanel
        src <- Session.getValue session
        send s src

      render ir = do
        old <- readRef pipelineRef
        writeRef pipelineRef Nothing
        case old of
          Nothing -> return unit
          Just p -> do
            trace "dispose old pipeline"
            disposePipeline p
        trace "allocate new pipeline"
        ppl <- allocPipeline ir
        trace "attach pipeline input"
        setPipelineInput ppl (Just pplInput)
        trace "generate object commands"
        sortSlotObjects pplInput
        writeRef pipelineRef $ Just ppl

  socket1 <- webSocket "ws://localhost:8000/exerciselist" $
    { onOpen    : \s -> do
        send s "query"
        trace "socket1 is ready"
        return unit
    , onMessage : \s m -> case A.jsonParser m >>= A.decodeJson of
          Left e -> trace $ "decode error: " ++ e
          Right es -> do
            for_ (es :: [String]) $ \e -> do
                op <- J.create "<option></option>"
                J.setText e op
                J.append op exerciseselect
            return unit
    , onError   : \s m -> trace m
    , onClose   : trace "socket1 is closed"
    }

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
        trace "got response"
        case A.jsonParser m >>= A.decodeJson of
          Left e -> trace $ "decode error: " ++ e
          Right (MyLeft (TypeInfo e) infos) -> do
            J.setText "Error" statuspanel
            J.setText e.text messagepanel
            range <- Range.create (e.startLine - 1) (e.startColumn - 1) (e.endLine - 1) (e.endColumn - 1)
            rs <- readRef markerRef
            for_ rs $ \mkr -> Session.removeMarker mkr session
            mkr <- addMarker range "lc_error" "text" false session
            writeRef markerRef [mkr]
            writeRef typeInfoRef infos
            return unit
          Right (MyRight p infos) -> do
            J.setText "Compiled" statuspanel
            J.setText "No errors." messagepanel
            writeRef typeInfoRef infos
            rs <- readRef markerRef
            for_ rs $ \mkr -> Session.removeMarker mkr session
            writeRef markerRef []
            render p
    , onError   : \s m -> trace $ "error: " ++ m
    }
  case socket of
    Left m -> trace $ "error: " ++ m
    Right ws -> do

      socket2 <- webSocket "ws://localhost:8000/getexercise" $
        { onOpen    : \s -> do
            trace "socket2 is ready"
            flip (J.on "change") exerciseselect $ \_ _ -> do
                sel <- J.find ":selected" exerciseselect
                txt <- J.getText sel
                send s txt
            return unit
        , onMessage : \s m -> case A.jsonParser m >>= A.decodeJson of
              Left e -> trace $ "decode error: " ++ e
              Right src -> do
                Session.setValue src session
                compile ws
                return unit
        , onError   : \s m -> trace m
        , onClose   : trace "socket2 is closed"
        }

      flip (J.on "click") btnCompile $ \_ _ -> do
        compile ws
        trace "clicked compile"
      addCommand editor "Compile" "Ctrl-B" "Command-B" (\_ -> compile ws)
      let renderLoop = do
            Milliseconds t <- nowEpochMilliseconds
            updateInput ((t - baseTime) / 1000)
            mppl <- readRef pipelineRef
            case mppl of
              Nothing -> return unit
              Just ppl -> renderPipeline ppl
            timeout (1000/25) renderLoop
      -- render loop
      renderLoop
      return unit
