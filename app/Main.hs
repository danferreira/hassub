{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad         (when)
import           Hassub
import           System.Console.Docopt
import           System.Directory
import           System.Environment    (getArgs)
import           System.Exit           (exitSuccess)

doc :: Docopt
doc = [docopt|
Hassub

Usage:
  hassub -l=LANG [-s] --all
  hassub -l=LANG [-s] <file>...
  hassub --help
  hassub --version

Options:
  -l=LANG                    Subtitle language. Use ISO 639-2 codes e.g. eng (use pob to portugues br)
  --all                      Download subtitles for all movie files in current directory
  -s, --silent               Non interactive mode. Just download the subtitles with more downloads count
  -h, --help                 Show this help text
  -v, --version              Show version
|]

version :: String
version = "Hassub Version 0.1"

main :: IO ()
main = do
        args <- getArgs >>= parseArgsOrExit doc

        when (isPresent args (shortOption 'h')) $ exitWithUsage doc
        when (isPresent args (shortOption 'v')) $ putStrLn version >> exitSuccess

        let (Just lang) = args `getArg` shortOption 'l'
            silent = isPresent args (shortOption 's')

        files <- getFiles args

        getSubtitles lang silent files

getFiles :: Arguments -> IO [String]
getFiles args = if isPresent args (longOption "all") then do
              contents <- listDirectory "."
              return $ filter (\f -> getExtension f `elem` supportedExt) contents
           else
             return $ args `getAllArgs` argument "file"
           where
             getExtension = reverse . takeWhile (/= '.') . reverse

supportedExt :: [String]
supportedExt = ["mkv", "avi", "mp4"]
