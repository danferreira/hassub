module OpenSubtitles.API where

import           Network.XmlRpc.Client

import qualified OpenSubtitles.Download as D
import qualified OpenSubtitles.Login    as L
import qualified OpenSubtitles.Logout   as LO
import qualified OpenSubtitles.Search   as S

type Token = String
type SubtitleId = String

--Endpoints

server = "http://api.opensubtitles.org/xml-rpc"

login :: L.LoginRequest -> IO L.LoginResponse
login (L.LoginRequest u p i ua) = remote server "LogIn" u p i ua

search :: Token -> [S.SearchRequest] -> IO S.SearchResponse
search = remote server "SearchSubtitles"

download :: Token -> [SubtitleId] -> IO D.DownloadResponse
download = remote server "DownloadSubtitles"

logout :: Token -> IO LO.LogoutResponse
logout = remote server "LogOut"

--Endpoints
