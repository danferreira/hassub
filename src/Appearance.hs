module Appearance where

import System.Console.ANSI

yellow :: String
yellow  = setSGRCode [SetColor Foreground Vivid Yellow]
cyan :: String
cyan  = setSGRCode [SetColor Foreground Vivid Cyan]
red :: String
red  = setSGRCode [SetColor Foreground Vivid Red]
green :: String
green = setSGRCode [SetColor Foreground Vivid Green]
blue :: String
blue  = setSGRCode [SetColor Foreground Vivid Blue]

bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

underline :: String
underline = setSGRCode [SetUnderlining SingleUnderline]

reset :: String
reset = setSGRCode [Reset]
