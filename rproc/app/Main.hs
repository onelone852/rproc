{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.MutMap (MutMap, empty, get, insert)
import Network.Rproc.Host
import Network.Rproc.Service (addImpureMethod, addMethod, constMethod, empty, (&))

helloworld :: () -> String
helloworld = constMethod "Hello, world!"

numberSum :: [Int] -> Int
numberSum = Prelude.sum

main :: IO ()
main = do
  mmap <- Data.MutMap.empty :: IO (MutMap String Int)
  let getNum key = fromMaybe 0 <$> get key mmap
  let putNum (key, i) = insert key i mmap
  let service =
        Network.Rproc.Service.empty
          & addMethod "helloworld" helloworld
          & addMethod "helloworld2" helloworld
          & addMethod "sum" numberSum
          & addImpureMethod "putnum" putNum
          & addImpureMethod "getnum" getNum
  let host =
        newMultiServicesHost 8080
          & addService "/" service
  putStrLn "Starting..."
  run host
