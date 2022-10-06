{-# LANGUAGE LambdaCase #-}

module TCP.Client (runClient) where

import Control.Monad (forever)
import Control.Monad.Extra (ifM, whenM)
import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Network.Simple.TCP
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, isEOF, stdout)

runClient :: IO ()
runClient = connect "10.10.17.199" "9000" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connected to " ++ show remoteAddr
  forever $ do
    prompt >>= \case
      Nothing -> disconnect connectionSocket
      Just input -> send connectionSocket $ fromString input
    recv connectionSocket 4096 >>= reportMsg

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
