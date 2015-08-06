module WebSocket where

import Prelude
import Control.Monad.Eff
import Data.Either

foreign import data WS :: !
foreign import data Socket :: *

type WSAction eff a = Eff (ws :: WS | eff) a

type WebSocketHandler eff =
  { onOpen    :: Socket -> WSAction eff Unit
  , onClose   :: WSAction eff Unit
  , onMessage :: Socket -> String -> WSAction eff Unit
  , onError   :: Socket -> String -> WSAction eff Unit
  }

foreign import webSocketImpl :: forall eff . String -> WebSocketHandler eff -> (Socket -> Either String Socket) -> (String -> Either String Socket) -> WSAction eff (Either String Socket)

webSocket :: forall eff . String -> WebSocketHandler eff -> WSAction eff (Either String Socket)
webSocket a b = webSocketImpl a b Right Left

foreign import send :: forall eff . Socket -> String -> WSAction eff Unit
