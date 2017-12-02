module Appearance (yellow, cyan, red, green, blue, bold) where

import           System.Console.ANSI

yellow :: String -> String
yellow = setColor Yellow

cyan :: String -> String
cyan = setColor Cyan

red :: String -> String
red = setColor Red

green :: String -> String
green = setColor Green

blue :: String -> String
blue = setColor Blue

bold :: String -> String
bold s = setSGRCode [SetConsoleIntensity BoldIntensity] ++ s ++ setSGRCode [Reset]

setColor :: Color -> String -> String
setColor c s = setSGRCode [SetColor Foreground Vivid c] ++ s ++ setSGRCode [Reset]
