{-# LANGUAGE TemplateHaskell #-}

module OpenSubtitles.Login where

import           Network.XmlRpc.Internals
import           Network.XmlRpc.THDeriveXmlRpcType

data LoginRequest = LoginRequest {
  username  :: String
, password  :: String
, language  :: String
, useragent :: String
} deriving Show

data LoginResponse = LoginResponse {
  token  :: Maybe String,
  status :: String
} deriving Show

-- instance XmlRpcType LoginResponse where
--       fromValue l = do
--                     v <- fromValue l
--                     t <- getFieldMaybe "token" v
--                     s <- getField "status" v
--                     return LoginResponse { token = t, status = s }
--       getType _ = TStruct

$(asXmlRpcStruct ''LoginResponse)
