module Main where

import System.Environment         (getArgs)
import Control.Monad              (when)
import Data.Maybe                 (fromMaybe)

import Hassub
import Options

main :: IO ()
main = do

      (flags, inputFiles) <- parseOptions =<< getArgs

      when (null flags)
       $ die help
      when (elem Help flags)
       $ exit help
      when (elem Version flags)
       $ exit version

      when (null inputFiles || null (head inputFiles))
       $ die emptyFiles

      getSubtitle (fromMaybe "eng" (getLang flags)) (head inputFiles)
