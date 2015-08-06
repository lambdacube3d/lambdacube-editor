module Timer where

import Prelude
import Control.Monad.Eff

foreign import data TIMEOUT :: !
foreign import data Timer :: *

foreign import timeout :: forall eff a. 
                               Int -> 
                               Eff (timeout :: TIMEOUT | eff) a -> 
                               Eff (timeout :: TIMEOUT | eff) Timer

foreign import clearTimeout :: forall eff. 
                               Timer -> 
                               Eff (timeout :: TIMEOUT | eff) Unit

foreign import data NOW :: !

foreign import nowEpochMilliseconds :: forall e. Eff (now :: NOW | e) Number
