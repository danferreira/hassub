{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Appearance
import           Config
import           Control.Applicative
import           Control.Monad         (unless, when)
import           Data.Maybe
import qualified File                  as F
import           Hassub
import           System.Console.Docopt
import           System.Environment    (getArgs)
import           System.Exit           (exitSuccess)
import           Utils

doc :: Docopt
doc = [docopt|
Hassub

Usage:
  hassub -d [-l=LANG] [-s] --all
  hassub -d [-l=LANG] [-s] <file>...
  hassub --config
  hassub --help
  hassub --version

Options:
  -d, --download              Download the subtitle
  -l=LANG                     Subtitle language. Use ISO 639-2 codes e.g. eng
  -a, --all                   Download subtitles for all movie files in current directory
  -s, --silent                Non interactive mode. Just download the subtitles with more downloads count
  -c, --config                Edit the configuration file (default language, login and password)
  -h, --help                  Show this help text
  -v, --version               Show version
|]

version :: String
version = "Hassub Version 0.1"

main :: IO ()
main = do
        args <- getArgs >>= parseArgsOrExit doc
        when (isPresent args (shortOption 'h')) $ exitWithUsage doc
        when (isPresent args (shortOption 'v')) $ putStrLn version >> exitSuccess
        when (isPresent args (longOption "config")) $ setConfig >> exitSuccess
        cf <- getConfig
        let langMaybe = args `getArg` shortOption 'l' <|> get defaultLang cf
        when (isNothing langMaybe) $ putStrLn "Parameter --lang or default configuration not found" >> exitSuccess
        let lang = fromJust langMaybe
        unless (lang `elem` supportedLangs) $ putStrLn ("Unknown language: "++ yellow ++ lang ++ reset ++". Please, use "++green++"ISO 639-2"++reset++" codes") >> exitSuccess
        let silent = isPresent args (shortOption 's')
        files <- if isPresent args (longOption "all") then
                    F.getFilesInDirectory
                    else
                      return $ args `getAllArgs` argument "file"
        let user = fromMaybe "" $ get username cf
            pass = fromMaybe "" $ get password cf
        getSubtitles lang user pass silent files
        where
          get f cf = case cf of
                           Just c  -> Just (f c)
                           Nothing -> Nothing
