module OpenSubtitles.Login where

import Network.XmlRpc.Internals

data LoginRequest = LoginRequest { username :: String, password :: String, language :: String, useragent :: String } deriving Show

data LoginResponse = LoginResponse { token :: Maybe String, status :: String } deriving Show

instance XmlRpcType LoginRequest where
      toValue l = toValue $ [("username", toValue (username l)),
                             ("password", toValue (password l)),
                             ("language", toValue (language l)),
                             ("useragent", toValue (useragent l))]
      fromValue v = do
                    l <- fromValue v
                    u <- getField "username" l
                    p <- getField "password" l
                    lang <- getField "language" l
                    ua <- getField "useragent" l
                    return LoginRequest { username = u, password = p, language = lang, useragent = ua }
      getType _ = TStruct


instance XmlRpcType LoginResponse where
      -- toValue l = toValue $ [("token", toValue (token l)),
      --                        ("status", toValue (status l))]

      fromValue l = do
                    v <- fromValue l
                    t <- getFieldMaybe "token" v
                    s <- getField "status" v
                    return LoginResponse { token = t, status = s }
      getType _ = TStruct
