module Hash
    ( Hash
    , mkHash
    ) where

import Data.Digest.Pure.MD5 hiding (Hash)
import Data.ByteString.Lazy.UTF8 (fromString)

type Hash = MD5Digest

mkHash :: String -> Hash
mkHash = md5 . fromString


