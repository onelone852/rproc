{-# LANGUAGE GADTs #-}

module Network.Rproc.Service (
  -- Types that exported
  Service,
  CallError,
  -- Empty Service
  empty,
  -- Useful functions
  addMethod,
  addImpureMethod,
  toStatus,
  call,
  constMethod,
  -- Reexport
  (&),
  FromJSON,
  ToJSON,
) where

import Data.Aeson (FromJSON, Result (Success), ToJSON (toJSON), Value, fromJSON)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Network.HTTP.Types.Status as Status

data Method where
  Method :: (FromJSON a, ToJSON b) => (Value -> Maybe a) -> (a -> IO b) -> Method

newtype Service = Service (HM.HashMap Text Method)

instance Semigroup Service where
  (<>) (Service a) (Service b) = Service (a <> b)

instance Monoid Service where
  mempty = empty

data CallError = MethodNotFound | ParseFailed deriving (Eq)

instance Show CallError where
  show MethodNotFound = "Method Not Found"
  show ParseFailed = "Parameters Parse Failed"

toStatus :: CallError -> Status.Status
toStatus MethodNotFound = Status.status404
toStatus ParseFailed = Status.status400

constMethod :: a -> (() -> a)
constMethod = const

empty :: Service
empty = Service HM.empty

mayFromJson :: (FromJSON a) => Value -> Maybe a
mayFromJson val = case fromJSON val of
  Data.Aeson.Success a -> Just a
  _ -> Nothing

addMethod :: (FromJSON a, ToJSON b) => Text -> (a -> b) -> Service -> Service
addMethod key method = addImpureMethod key (return . method)

addImpureMethod :: (FromJSON a, ToJSON b) => Text -> (a -> IO b) -> Service -> Service
addImpureMethod key method (Service mmap) = Service $ HM.insert key (Method mayFromJson method) mmap

callLookup :: Text -> HM.HashMap Text v -> Either CallError v
callLookup name hmap = case HM.lookup name hmap of
  Just x -> Right x
  Nothing -> Left MethodNotFound

call :: Text -> Value -> Service -> Either CallError (IO Value)
call name obj (Service hmap) = do
  Method verifier method <- callLookup name hmap
  let result = verifier obj
  case result of
    Just val -> Right $ toJSON <$> method val
    _ -> Left ParseFailed
