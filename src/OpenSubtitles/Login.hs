{-# LANGUAGE TemplateHaskell #-}

module OpenSubtitles.Login where

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

$(asXmlRpcStruct ''LoginResponse)
