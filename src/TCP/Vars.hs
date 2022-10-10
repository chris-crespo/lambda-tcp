module TCP.Vars (host, port, msgSize) where

import Network.Simple.TCP (HostName, ServiceName)

host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "3000"

msgSize :: Int
msgSize = 4096
