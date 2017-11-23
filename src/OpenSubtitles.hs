module OpenSubtitles where

import Network.XmlRpc.Client

import Login
import Search
import Download

type Username = String
type Password = String
type SubLanguageId = String
type UserAgent = String
type Token = String
type SubtitleId = String

--Endpoints

server = "http://api.opensubtitles.org/xml-rpc"

login :: Username -> Password -> SubLanguageId -> UserAgent -> IO LoginResponse
login = remote server "LogIn"

search :: Token -> [SearchRequest] -> IO SearchResponse
search = remote server "SearchSubtitles"

download :: Token -> [SubtitleId] -> IO DownloadResponse
download = remote server "DownloadSubtitles"

--Endpoints

-- mountRequests :: SubLanguageId -> [String] -> IO [SearchRequest]
-- mountRequests lang files = files >>= (\f -> (getHashAndSize f) >>= (\(h, s) -> return SearchRequest f lang h s))

-- getFilename :: String -> [SearchRequest] -> Maybe String
-- getFilename hash l = case find (\(SearchRequest m _ h _-> hash == h) of
--                       Just (SearchRequest m _ _ _) -> Just m
--                       Nothing -> Nothing
--
-- getTopNSubsFromEach :: Int -> [SearchSubResponse] -> [SearchSubResponse]
-- getTopNSubsFromEach n l =
