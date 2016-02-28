-- | A very simple cache
module Cache
    ( Cache
    , newCache
    , lookupCache
    , clearCache
    ) where

import Hash

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import Data.Array.IO (IOArray, newArray, readArray, writeArray, getBounds)
import Data.Char (digitToInt)

-------------------------

data Cache a 
    = Cache
        { array         :: IOArray Int [CacheEntry a]
        , cacheLineSize :: Int  -- length of the lists
        }

data CacheEntry a
    = CacheEntry 
        { question :: Hash
        , answer   :: MVar a
        }


newCache :: Int -> IO (Cache a)
newCache x = do
    a <- newArray (0,255) []
    return $ Cache a x


clearCache :: Cache a -> IO ()
clearCache c = do
    (a,b) <- getBounds $ array c
    mapM_ (\i -> writeArray (array c) i []) [a..b]


lookupCache :: Cache a -> Hash -> IO (Either a (a -> IO ()))
lookupCache ch e = modifyCacheLine (array ch) (getIndex e) $ \vv ->
    case lookupIA (cacheLineSize ch) (\x -> e == question x) vv of
        (Just x_, c) -> do
            x <- readMVar (answer x_)
            return (x_ : c, Left x)
        (Nothing, c) -> do
            v <- newEmptyMVar
            return (CacheEntry e v: c, Right $ putMVar v)
 where
    lookupIA :: Int -> (a -> Bool) -> [a] -> (Maybe a, [a])
    lookupIA i p l = f i l  where
        f _ (x: xs) | p x = (Just x, xs)
        f 1 _  = (Nothing, [])
        f i (x: xs) = case f (i-1) xs of
            (a, b) -> (a, x:b)
        f _ [] = (Nothing, [])

    modifyCacheLine ch i f = do
        x <- readArray ch i
        (x', r) <- f x
        writeArray ch i x'
        return r

    getIndex :: Hash -> Int
    getIndex e = 16 * digitToInt a + digitToInt b where (a:b:_) = show e



