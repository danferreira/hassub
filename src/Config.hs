{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Appearance
import           Data.Aeson
import           Data.Hash.MD5
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           System.Directory (getHomeDirectory)
import           System.IO        (hFlush, stdout)

data Config = Config {
    defaultLang :: String,
    username    :: String,
    password    :: String
} deriving (Show, Generic)

instance FromJSON Config where
    parseJSON (Object m) = Config <$>
        m .: "defaultLang" <*>
        m .: "username" <*>
        m .: "password"
    parseJSON x = fail ("not an object: " ++ show x)

instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

configFile :: IO String
configFile = getHomeDirectory >>= \h -> return $ h++"/.config/hassub.yml"

getConfig :: IO (Maybe Config)
getConfig = do
            path <- configFile
            config <- decodeFileEither path
            case config of
              Right x -> return (Just x)
              Left _  -> return Nothing

setConfig :: IO ()
setConfig = do
            cf <- getConfig
            let cLang = fromMaybe "eng" $ get defaultLang cf
                cUser = fromMaybe "" $ get username cf
            putStrLn "Editing the configuration file.\n"
            putStrLn "Set a default language so you wont need to specify -l parameter everytime"
            putStr $ "Default Language" ++ yellow (" ["++ cLang ++"]: ")
            hFlush stdout
            l <- getLine
            let lang = if null l then cLang else l
            putStrLn $ "\nSet your login from " ++ green "opensubtitles.org" ++ ". Can be left blank if logging in anonymously."
            putStr "Username: "
            hFlush stdout
            user <- getLine
            pass <- if null user then
                       return ""
                       else do
                        putStr "Password: "
                        hFlush stdout
                        (md5s . Str) <$> getLine
            fp <- configFile
            encodeFile fp $ Config lang user pass
            putStrLn $"Configuration saved in: " ++ green fp
            where
              get f cf = case cf of
                           Just c  -> Just (f c)
                           Nothing -> Nothing
