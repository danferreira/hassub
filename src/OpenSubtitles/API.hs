module OpenSubtitles.API where

import Network.XmlRpc.Client

import qualified OpenSubtitles.Login as L
import qualified OpenSubtitles.Search as S
import qualified OpenSubtitles.Download as D

type Username = String
type Password = String
type SubLanguageId = String
type UserAgent = String
type Token = String
type SubtitleId = String

--Endpoints

server = "http://api.opensubtitles.org/xml-rpc"

login :: Username -> Password -> SubLanguageId -> UserAgent -> IO L.LoginResponse
login = remote server "LogIn"

search :: Token -> [S.SearchRequest] -> IO S.SearchResponse
search = remote server "SearchSubtitles"

download :: Token -> [SubtitleId] -> IO D.DownloadResponse
download = remote server "DownloadSubtitles"

--Endpoints
