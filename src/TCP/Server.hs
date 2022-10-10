module TCP.Server (runServer) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Eval.Env
import qualified Eval.Env as Env
import Eval.Lazy
import Network.Simple.TCP
import Parse
import Pretty
import TCP.Vars

repl :: Socket -> Env -> IO Env
repl sock env = do
  input <- receive sock msgSize
  case parse decl input of
    Left err -> send sock (B.pack $ show err) >> repl sock env
    Right decl' -> do
      let (expr', env') = eval decl' env
      send sock (B.pack $ pretty expr')
      repl sock env'

receive :: Socket -> Int -> IO String
receive sock size = B.unpack . fromMaybe B.empty <$> recv sock size

runServer :: IO ()
runServer = serve (Host host) port $ \(connectionSocket, remoteAddr) ->
  forever $ do
    putStrLn $ "Connection established to " ++ show remoteAddr
    repl connectionSocket Env.empty
