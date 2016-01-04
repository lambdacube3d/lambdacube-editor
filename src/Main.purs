module Main (main,run) where

import Prelude
import Extensions (fail)

import qualified Control.Monad.Eff.Console as C

import Control.Bind
import TypeInfo
import Timer
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Foldable (for_, foldl)

import Ace
import Ace.Types
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session
import qualified Ace.Range as Range

import qualified Graphics.WebGL as GL
import qualified Control.Monad.Eff.JQuery as J

import WebSocket
import DefaultText
import Data.Either
import Sample

import Data.Function
import Data.Maybe
import Data.StrMap(fromList,StrMap(..))
import Data.List(toList)
import Data.Tuple
import Data.Array
import Data.Int

import Backend
import IR
import LinearBase
import Mesh
import Type
import Input

--import MeshJsonDecode
--import PipelineJsonDecode
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import qualified Data.Argonaut.Core as AC


import Data.Matrix (Mat(..))
import Data.Matrix4
import Data.Vector3

import qualified DOM as DOM

main :: forall e. Eff (console :: C.CONSOLE | e) Unit
main = do
  C.log "Start LambdaCube 3D Editor"
--main :: forall m. (Applicative m) => m Unit
--main = return unit
--run :: forall m. (Applicative m) => m Unit
--run = return unit

foreign import getMousePos :: forall eff a. J.JQueryEvent -> Eff (dom :: DOM.DOM | eff) (Array Number)
foreign import getJSON :: forall eff a. String -> (AC.Json -> Eff (dom :: DOM.DOM | eff) a) -> Eff (dom :: DOM.DOM | eff) Unit

--  control-b - compile/build
--  control-n - new

type TypeInfoRecord =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  , text :: String
  }

foreign import addCommand :: forall eff . Editor -> String -> String -> String -> (Editor -> Eff (ace :: ACE | eff) Unit) -> Eff (ace :: ACE | eff) Unit
foreign import tokenTooltip :: forall eff . Editor -> (Int -> Int -> Eff (ace :: ACE | eff) TypeInfoRecord) -> Eff (ace :: ACE | eff) Unit
foreign import addMarkerImpl :: forall eff. Fn5 Range String String Boolean EditSession (Eff (ace :: ACE | eff) Int)

addMarker :: forall eff. Range -> String -> String -> Boolean -> EditSession -> Eff (ace :: ACE | eff) Int
addMarker range clazz _type inFront self = runFn5 addMarkerImpl range clazz _type inFront self

--fromArray :: forall a f. (Foldable f) => f (Tuple String a) -> StrMap a
fromArray :: forall a. Array (Tuple String a) -> StrMap a
fromArray a = fromList (toList a)

run = GL.runWebGL "glcanvas" (\s -> C.log s) $ \context -> do
  baseTime <- nowEpochMilliseconds
  -- setup pipeline input
  let inputSchema = 
        { slots : fromArray [ Tuple "stream"  {primitive: Triangles, attributes: fromArray [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                            , Tuple "stream4" {primitive: Triangles, attributes: fromArray [Tuple "position4" TV4F, Tuple "vertexUV" TV2F]}
                            , Tuple "line"    {primitive: Triangles, attributes: fromArray [Tuple "position" TV3F]}
                            , Tuple "grid"    {primitive: Triangles, attributes: fromArray [Tuple "position" TV3F]}
                            , Tuple "grid3d"  {primitive: Points,    attributes: fromArray [Tuple "position" TV3F]}
                            , Tuple "quad"    {primitive: Triangles, attributes: fromArray [Tuple "position" TV2F]}
                            , Tuple "cube"    {primitive: Triangles, attributes: fromArray [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                            ]
        , uniforms : fromArray [Tuple "MVP" M44F, Tuple "MVP2" M44F, Tuple "Time" Float, Tuple "Mouse" V2F]
        }
  pplInput <- mkWebGLPipelineInput inputSchema

  let toLCMat4 :: Mat4 -> M44F
      toLCMat4 (Mat [x11, x21, x31, x41, x12, x22, x32, x42, x13, x23, x33, x43, x14, x24, x34, x44]) = let
          v1 = V4 x11 x21 x31 x41
          v2 = V4 x12 x22 x32 x42
          v3 = V4 x13 x23 x33 x43
          v4 = V4 x14 x24 x34 x44
        in V4 v1 v2 v3 v4
      toLCMat4 _ = fail "invalid Mat4"

      updateInput t = do
        w <- GL.getCanvasWidth context
        h <- GL.getCanvasHeight context
        setScreenSize pplInput (V2 w h)
        let pi = 3.141592653589793
            angle = pi / 24.0 * t
            cm = makeLookAt (vec3 3.0 1.3 0.3) (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)
            mm = makeRotate angle (vec3 0.0 1.0 0.0)
            pm = makePerspective 30.0 (toNumber w / toNumber h) 0.1 100.0
            mvp = pm `mulM` cm `mulM` mm
            cm' = makeLookAt (vec3 4.0 0.5 (-0.6)) (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)
            mvp2 = pm `mulM` cm' `mulM` mm

        uniformFloat "Time" pplInput.uniformSetter t
        uniformM44F "MVP" pplInput.uniformSetter $ toLCMat4 mvp
        uniformM44F "MVP2" pplInput.uniformSetter $ toLCMat4 mvp2

  gpuCube <- compileMesh myCube
  addMesh pplInput "stream4" gpuCube []

  gpuQuad <- compileMesh myQuad
  addMesh pplInput "quad" gpuQuad []

  let addRemoteModel sname uri = getJSON uri $ \m -> do
        case decodeJson m of
          Left e -> C.log $ "decode error: " ++ e
          Right (mesh) -> do
            gpuMesh <- compileMesh mesh
            addMesh pplInput sname gpuMesh []
            sortSlotObjects pplInput
            return unit

  --addRemoteModel "grid3d" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/grid3d.mesh.json"
  --addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/logo2.mesh.json"
  --addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/logo3.mesh.json"

  --addRemoteModel "quad"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/quad.mesh.json"
  --addRemoteModel "grid"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/grid.mesh.json"
  --addRemoteModel "line"   "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/line.mesh.json"
  --addRemoteModel "stream" "http://rawgit.com/lambdacube3d/lambdacube-editor/master/mesh/ico.mesh.json"

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
    uniformV2F "Mouse" pplInput.uniformSetter $ V2 (x / toNumber w) (y / toNumber h)

  pipelineRef <- newRef Nothing
  let compile s = do
        C.log "compile"
        J.setText "Compiling..." statuspanel
        src <- Session.getValue session
        send s src

      render ir = do
        old <- readRef pipelineRef
        writeRef pipelineRef Nothing
        case old of
          Nothing -> return unit
          Just p -> do
            C.log "dispose old pipeline"
            disposePipeline p
        C.log "allocate new pipeline"
        ppl <- allocPipeline ir
        C.log "attach pipeline input"
        setPipelineInput ppl (Just pplInput)
        C.log "generate object commands"
        sortSlotObjects pplInput
        writeRef pipelineRef $ Just ppl

  socket1 <- webSocket "ws://localhost:8000/exerciselist" $
    { onOpen    : \s -> do
        send s "query"
        C.log "socket1 is ready"
        return unit
    , onMessage : \s m -> case jsonParser m >>= decodeJson of
          Left e -> C.log $ "decode error: " ++ e
          Right es -> do
            for_ (es :: Array String) $ \e -> do
                op <- J.create "<option></option>"
                J.setText e op
                J.append op exerciseselect
            return unit
    , onError   : \s m -> C.log m
    , onClose   : C.log "socket1 is closed"
    }

  socket <- webSocket "ws://localhost:8000/compile" $
    { onOpen    : \s -> do
        C.log "socket is ready"
        compile s
        timerRef <- newRef Nothing
        Session.onChange session $ do
          t <- readRef timerRef
          case t of
            Nothing -> return unit
            Just a -> clearTimeout a
          writeRef timerRef =<< Just <$> timeout 1000 (compile s)

    , onClose   : C.log "socket is closed"
    , onMessage : \s m -> do
        C.log "got response"
        case jsonParser m >>= decodeJson of
          Left e -> C.log $ "decode error: " ++ e
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
    , onError   : \s m -> C.log $ "error: " ++ m
    }
  case socket of
    Left m -> C.log $ "error: " ++ m
    Right ws -> do

      socket2 <- webSocket "ws://localhost:8000/getexercise" $
        { onOpen    : \s -> do
            C.log "socket2 is ready"
            flip (J.on "change") exerciseselect $ \_ _ -> do
                sel <- J.find ":selected" exerciseselect
                txt <- J.getText sel
                send s txt
            return unit
        , onMessage : \s m -> case jsonParser m >>= decodeJson of
              Left e -> C.log $ "decode error: " ++ e
              Right src -> do
                Session.setValue src session
                compile ws
                return unit
        , onError   : \s m -> C.log m
        , onClose   : C.log "socket2 is closed"
        }

      flip (J.on "click") btnCompile $ \_ _ -> do
        compile ws
        C.log "clicked compile"
      addCommand editor "Compile" "Ctrl-B" "Command-B" (\_ -> compile ws)
      
      let renderLoop = do
            t <- nowEpochMilliseconds
            updateInput ((t - baseTime) / 1000.0)
            mppl <- readRef pipelineRef
            case mppl of
              Nothing -> return unit
              Just ppl -> renderPipeline ppl
            timeout (1000/25) renderLoop
      -- render loop
      renderLoop
      
      return unit
