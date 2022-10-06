module TCP.Server (runServer) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Data.String
import Eval.Env
import qualified Eval.Env as Env
import Eval.Lazy
import Network.Simple.TCP
import Parse
import Pretty

repl :: Socket -> Env -> IO Env
repl sock env = do
  input <- B.unpack . fromMaybe B.empty <$> recv sock 4096
  putStrLn $ "Received: " ++ input
  case parse decl input of
    (Left err, _) -> send sock (B.pack $ show err) >> repl sock env
    (Right decl', _) -> case eval decl' env of
      (Nothing, env') -> print "eval" >> send sock (B.pack $ show decl') >> repl sock env'
      (Just expr', env') -> send sock (B.pack $ pretty expr') >> repl sock env'

runServer :: IO ()
runServer = serve (Host "10.10.17.212") "3000" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connected: " ++ show remoteAddr
  forever $ repl connectionSocket Env.empty
