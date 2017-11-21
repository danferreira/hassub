module Options where

import System.Console.GetOpt
import System.IO                  (hPutStrLn, stderr, stdout)
import System.Exit                (ExitCode (ExitSuccess, ExitFailure), exitWith)

data Flag = Version
            | Help
            | Language String
  deriving (Show, Eq)

version :: String
version = "Version 1.0"

emptyFiles :: String
emptyFiles = "Missing files"

options :: [OptDescr Flag]
options =
        [ Option "h" ["help"]    (NoArg Help)                 "Show this help"
        , Option "v" ["version"] (NoArg Version)              "Print Hassub version"
        , Option "l" ["language"](ReqArg Language "LANG")     "Language"
        ]

getLang :: [Flag] -> Maybe String
getLang flags = case [x | Language x <- flags] of
            [] -> Nothing
            [""] -> Nothing
            [x] -> Just x

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ help))

help :: String
help = usageInfo usageHeader options
    where
      usageHeader = "Usage: hassub [options...] [files...]\n\nAvailable options:"

exit :: String -> IO ()
exit msg = do
  hPutStrLn stdout msg
  exitWith ExitSuccess

die :: String -> IO ()
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)
