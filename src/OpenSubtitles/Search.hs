module OpenSubtitles.Search where

import Network.XmlRpc.Internals


data SearchRequest = SearchRequest { filename :: String, subLanguageId :: String, movieHash :: String, movieByteSize :: Double } deriving Show

data SearchResponse = SearchResponse { status :: String, result :: [SearchSubResponse] } deriving Show

data SearchSubResponse = SearchSubResponse { idSubtitleFile :: String, subFilename :: String, subRating :: String, subDownloadsCnt :: String } deriving Show


instance XmlRpcType SearchRequest where
      toValue l = toValue $ [("sublanguageid", toValue (subLanguageId l)),
                           ("moviehash", toValue (movieHash l)),
                           ("moviebytesize", toValue (movieByteSize l))]
      getType _ = TStruct

instance XmlRpcType SearchResponse where
      fromValue res = do
                  v <- fromValue res
                  s <- getField "status" v
                  d <- getField "data" v
                  return SearchResponse { status = s, result = d }
      getType _ = TStruct

instance XmlRpcType SearchSubResponse where
      fromValue res = do
                    v <- fromValue res
                    s <- getField "SubFileName" v
                    i <- getField "IDSubtitleFile" v
                    r <- getField "SubRating" v
                    c <- getField "SubDownloadsCnt" v
                    return SearchSubResponse { idSubtitleFile = i, subFilename = s, subRating = r, subDownloadsCnt = c }
      getType _ = TStruct
