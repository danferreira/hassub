module OpenSubtitles.Logout where

import Network.XmlRpc.Internals

data LogoutResponse = LogoutResponse {
  status :: String
} deriving Show

instance XmlRpcType LogoutResponse where
      fromValue l = do
                    v <- fromValue l
                    s <- getField "status" v
                    return LogoutResponse { status = s }
      getType _ = TStruct
