module Data.MutMap (
  -- Type that exported
  MutMap,
  -- Function that interact with HashMap and other data structure
  fromHashMap,
  fromList,
  empty,
  toHashMap,
  toList,
  rewrite,
  modify,
  -- Functions that impl map interface
  insert,
  delete,
  get,
  getUnsafe,
  has,
  Data.MutMap.null,
  size,
  act,
  -- Symbol functions
  (!),
  (?!),
) where

import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Mapm
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)

newtype MutMap k v = MutMap (IORef (HashMap k v))

fromHashMap :: HashMap k v -> IO (MutMap k v)
fromHashMap hmap = do
  ref_map <- newIORef hmap
  return $ MutMap ref_map

fromList :: (Hashable k) => [(k, v)] -> IO (MutMap k v)
fromList = fromHashMap . Mapm.fromList

toHashMap :: MutMap k v -> IO (HashMap k v)
toHashMap (MutMap ref) = readIORef ref

toList :: MutMap k v -> IO [(k, v)]
toList rmap = Mapm.toList <$> toHashMap rmap

empty :: IO (MutMap k v)
empty = fromHashMap Mapm.empty

rewrite :: HashMap k v -> MutMap k v -> IO ()
rewrite hmap (MutMap ref) = writeIORef ref hmap

insert :: (Eq k, Hashable k) => k -> v -> MutMap k v -> IO ()
insert key val = modify (Mapm.insert key val)

delete :: (Eq k, Hashable k) => k -> MutMap k v -> IO (Maybe v)
delete key rmap = do
  val <- get key rmap
  when (isJust val) (modify (Mapm.delete key) rmap)
  return val

get :: (Eq k, Hashable k) => k -> MutMap k v -> IO (Maybe v)
get key = act $ Mapm.lookup key

getUnsafe :: (Eq k, Hashable k) => k -> MutMap k v -> IO v
getUnsafe key rmap = fromMaybe (error "getUnsafe: Nothing") <$> get key rmap

(?!) :: (Eq k, Hashable k) => MutMap k v -> k -> IO (Maybe v)
(?!) rmap key = get key rmap

(!) :: (Eq k, Hashable k) => MutMap k v -> k -> IO v
(!) rmap key = getUnsafe key rmap

null :: MutMap k v -> IO Bool
null = act Mapm.null

size :: MutMap k v -> IO Int
size = act Mapm.size

has :: (Eq k, Hashable k) => k -> MutMap k v -> IO Bool
has key = act $ Mapm.member key

modify :: (HashMap k v -> HashMap k v) -> MutMap k v -> IO ()
modify fn (MutMap ref) = modifyIORef ref fn

act :: (HashMap k v -> a) -> MutMap k v -> IO a
act fn (MutMap ref) = fn <$> readIORef ref
