module Main (main,run) where

import Prelude
import Extensions (fail)
import Global

import qualified Control.Monad.Eff.Console as C

import Control.Bind
import TypeInfo
import Timer
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Foldable (for_, foldl)
import Data.Traversable (for)

import Ace
import Ace.Types as Ace
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session
import qualified Ace.Range as Range

import qualified Graphics.WebGL as GL
import qualified Control.Monad.Eff.JQuery as J

import WebSocket
import Data.Either
import Sample

import Data.Foreign
import Data.Function
import Data.Maybe
import Data.StrMap(fromFoldable,StrMap(..))
import Data.Tuple
import Data.Array
import Data.Int
import qualified Data.StrMap as StrMap

import Backend
import IR
import LinearBase
import Mesh
import Type
import Input
import Data (uploadTexture2DToGPU)

import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import qualified Data.Argonaut.Core as AC


import Data.Matrix (Mat(..))
import Data.Matrix4
import Data.Vector3

import qualified DOM as DOM

defaultExampleName = "LambdaCube2.lc"

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
foreign import addMarkerImpl :: forall eff. Fn5 Ace.Range String String Boolean EditSession (Eff (ace :: ACE | eff) Int)

addMarker :: forall eff. Ace.Range -> String -> String -> Boolean -> EditSession -> Eff (ace :: ACE | eff) Int
addMarker range clazz _type inFront self = runFn5 addMarkerImpl range clazz _type inFront self

run = GL.runWebGL "glcanvas" (\s -> C.log s) $ \context -> do
  -- setup pipeline input
  let inputSchema = 
        { slots : fromFoldable
                    [ Tuple "stream"  {primitive: Triangles, attributes: fromFoldable [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                    , Tuple "stream4" {primitive: Triangles, attributes: fromFoldable [Tuple "position4" TV4F, Tuple "vertexUV" TV2F]}
                    , Tuple "line"    {primitive: Triangles, attributes: fromFoldable [Tuple "position" TV3F]}
                    , Tuple "grid"    {primitive: Triangles, attributes: fromFoldable [Tuple "position" TV3F]}
                    , Tuple "grid3d"  {primitive: Points,    attributes: fromFoldable [Tuple "position" TV3F]}
                    , Tuple "quad"    {primitive: Triangles, attributes: fromFoldable [Tuple "position" TV2F]}
                    , Tuple "cube"    {primitive: Triangles, attributes: fromFoldable [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                    , Tuple "lambdaCube" {primitive: Triangles, attributes: fromFoldable [Tuple "position"  TV3F, Tuple "normal" TV3F]}
                    ]
        , uniforms : fromFoldable [Tuple "MVP" M44F, Tuple "Time" Float, Tuple "Mouse" V2F, Tuple "Diffuse" FTexture2D, Tuple "OcclusionFieldMin" FTexture2D, Tuple "OcclusionFieldMax" FTexture2D]
        }
  pplInput <- mkWebGLPipelineInput inputSchema

  timeRef <- newRef 0.0
  b <- J.body
  timeBox <- J.find "#timeBox" b
  timeRange <- J.find "#timeRange" b
  let getTime = do
                  t <- J.getValue timeBox
                  case readNumber t of
                    Right time -> return time
                    _ -> case readString t of
                        Right timeStr -> return $ readFloat timeStr
                        _ -> return 0.0
  flip (J.on "input") timeBox $ \_ _ -> do
    J.getValue timeBox >>= flip J.setValue timeRange
    getTime >>= writeRef timeRef
  flip (J.on "input") timeRange $ \_ _ -> do
    J.getValue timeRange >>= flip J.setValue timeBox
    getTime >>= writeRef timeRef
  flip (J.on "change") timeBox $ \_ _ -> do
    J.getValue timeBox >>= flip J.setValue timeRange
    getTime >>= writeRef timeRef
  flip (J.on "change") timeRange $ \_ _ -> do
    J.getValue timeRange >>= flip J.setValue timeBox
    getTime >>= writeRef timeRef

  pauseBox <- J.find "#pause" b
  pipelinepanel <- J.find "#pipeline" b
  desugaredpanel <- J.find "#desugared" b
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

        uniformFloat "Time" pplInput.uniformSetter t
-- todo        uniformFloat "Aspect" pplInput.uniformSetter (toNumber w / toNumber h)

  gpuCube <- compileMesh myCube
  addMesh pplInput "stream4" gpuCube []

  gpuQuad <- compileMesh myQuad
  addMesh pplInput "quad" gpuQuad []

  gpuLambdaCube <- compileMesh lambdaCube
  addMesh pplInput "lambdaCube" gpuLambdaCube []

  -- upload textures
  uploadTexture2DToGPU "logo.png" (uniformFTexture2D "Diffuse" pplInput.uniformSetter)
  uploadTexture2DToGPU "OcclusionFieldMin.png" (uniformFTexture2D "OcclusionFieldMin" pplInput.uniformSetter)
  uploadTexture2DToGPU "OcclusionFieldMax.png" (uniformFTexture2D "OcclusionFieldMax" pplInput.uniformSetter)

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
  typeInfoRef <- newRef []
  markerRef <- newRef []
  let lessEqPos l c l' c' = l < l' || l == l' && c <= c'
  let lessPos l c l' c' = l < l' || l == l' && c < c'
  let getTypeInfo l c = do
        x <- readRef typeInfoRef
        let flattenTypeInfo (TypeInfo i) = case i.range of
              Range r ->
                { startLine   : r.startLine
                , startColumn : r.startColumn
                , endLine     : r.endLine
                , endColumn   : r.endColumn
                , text        : i.text
                }
            ps = flip filter (map flattenTypeInfo x) $ \ti -> lessEqPos ti.startLine ti.startColumn l c && lessPos l c ti.endLine ti.endColumn
            f Nothing i = Just i
            f (Just d) i
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

  pipelineRef <- newRef Nothing
  let compile s = do
        C.log "compile"
        J.setText "Compiling..." statuspanel
        src <- Session.getValue session
        send s src
{-
      loadUserTextures (Pipeline p) = do
        let isFTexture2D unis n = case StrMap.lookup n unis of
              Just FTexture2D -> true
              _ -> false
        for_ (concatMap (\(Slot s) -> filter (isFTexture2D s.slotUniforms) $ StrMap.keys s.slotUniforms) p.slots) $ \n ->
          uploadTexture2DToGPU n >>= uniformFTexture2D n pplInput.uniformSetter
-}
      render ir = do
        --loadUserTextures ir
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
          Right (CompileError ranges txt infos) -> do
            J.setText "Error" statuspanel
            J.setText txt messagepanel
            rs <- readRef markerRef
            for_ rs $ \mkr -> Session.removeMarker mkr session
            mkr <- for ranges $ \(Range e) -> do
                range <- Range.create (e.startLine - 1) (e.startColumn - 1) (e.endLine - 1) (e.endColumn - 1)
                addMarker range "lc_error" "text" false session
            writeRef markerRef mkr
            writeRef typeInfoRef infos
            return unit
          Right (Compiled dsSrc pplSrc p infos) -> do
            J.setText dsSrc desugaredpanel
            J.setText pplSrc pipelinepanel
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
            -- get the default example
            send s defaultExampleName
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

      lastTimeRef <- newRef =<< nowEpochMilliseconds
      let renderLoop = do
            t <- nowEpochMilliseconds
            lastTime <- readRef lastTimeRef
            let deltaTime = (t - lastTime) / 1000.0
            writeRef lastTimeRef t
            paused <- J.getProp "checked" pauseBox
            case readBoolean paused of
              Right false -> do
                  modifyRef timeRef (deltaTime +)
                  time <- readRef timeRef
                  let time' = (toNumber $ floor $ time * 1000.0)/1000.0
                  J.setValue time' timeBox
                  J.setValue time' timeRange
                  updateInput time
              _ -> do
                  time <- getTime
                  writeRef timeRef time
                  updateInput time
            mppl <- readRef pipelineRef
            case mppl of
              Nothing -> return unit
              Just ppl -> renderPipeline ppl
            timeout (1000/25) renderLoop
      -- render loop
      renderLoop
      
      return unit
