{-# LANGUAGE LambdaCase #-}

module TCP.Client (runClient) where

import Control.Monad (forever, unless)
import Control.Monad.Extra (ifM)
import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Network.Simple.TCP
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, isEOF, stdout)
import TCP.Vars

runClient :: IO ()
runClient = connect host port $ \(connectionSocket, _) ->
  forever $ do
    prompt >>= \case
      Nothing -> disconnect connectionSocket
      Just input -> unless (null input) $ do
        send connectionSocket $ fromString input
        recv connectionSocket msgSize >>= reportMsg

prompt :: IO (Maybe String)
prompt = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  ifM isEOF (return Nothing) (Just <$> getLine)

disconnect :: Socket -> IO ()
disconnect sock = do
  closeSock sock
  putStrLn "Disconnected"
  exitSuccess

reportMsg :: Maybe B.ByteString -> IO ()
reportMsg Nothing = return ()
reportMsg (Just msg) = B.putStrLn msg
