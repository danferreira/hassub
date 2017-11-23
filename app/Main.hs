{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Environment         (getArgs)
import Control.Monad              (when)
import System.Exit (exitSuccess)

import System.Console.Docopt
import Hassub (getSubtitle)

doc :: Docopt
doc = [docopt|
Hassub

Usage:
  hassub -l=LANG --all
  hassub -l=LANG <file>...
  hassub --help | --version

Options:
  -l=LANG                    Subtitle language. Use ISO 639-2 codes e.g. eng (use pob to portugues br)
  --all                      Download subtitles for all movie files in current directory
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
  when (isPresent args (longOption "all")) $ putStrLn "not implemented yet :(" >> exitSuccess

  let files = args `getAllArgs` (argument "file")
  let (Just lang) = args `getArg` (shortOption 'l')

  getSubtitle lang (head files)
