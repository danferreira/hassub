module OpenSubtitles.Download where

import           Network.XmlRpc.Internals

data DownloadResponse = DownloadResponse {
    status :: String
  , result :: [DownloadSubResponse]
 } deriving (Show)

data DownloadSubResponse = DownloadSubResponse {
  idsubtitlefile :: String
, data_          :: String
} deriving (Show)


instance XmlRpcType DownloadResponse where
      toValue l = toValue [("status", toValue (status l)),
                           ("data", toValue (result l))]
      fromValue s = do
                  v <- fromValue s
                  s <- getField "status" v
                  d <- getField "data" v
                  return DownloadResponse { status = s, result = d }
      getType _ = TStruct

instance XmlRpcType DownloadSubResponse where
      toValue l = toValue [("idsubtitlefile", toValue (idsubtitlefile l)),
                           ("data", toValue (data_ l))]
      fromValue l = do
                    v <- fromValue l
                    s <- getField "idsubtitlefile" v
                    d <- getField "data" v
                    return DownloadSubResponse { idsubtitlefile = s, data_ = d }
      getType _ = TStruct
