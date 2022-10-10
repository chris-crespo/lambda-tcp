{-# LANGUAGE LambdaCase #-}

module Main (main) where

import TCP.Client
import TCP.Server
import System.Environment

main :: IO ()
main =
  getArgs >>= \case
    ["client"] -> runClient
    ["server"] -> runServer
    _ -> putStrLn "Usage: lambda-tcp [client|server]"
