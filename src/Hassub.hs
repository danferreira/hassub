module Hassub (getSubtitle) where

import Data.List                  (find, sortBy)
import Control.Monad              (when)
import Data.Char                  (isDigit)
import Data.Maybe                 (fromJust)
import System.IO                  (hPutStrLn, stderr, stdout)
import System.Exit                (ExitCode (ExitSuccess, ExitFailure), exitWith)

import OpenSubtitles.API
import qualified OpenSubtitles.Login as L
import qualified OpenSubtitles.Search as S
import qualified OpenSubtitles.Download as D
import qualified File as F

userAgent :: String
userAgent = "myapp"

getSubtitle :: SubLanguageId -> String -> IO ()
getSubtitle lang file = do
    putStrLn $ "Searching for " ++ file ++ " in " ++ lang

    exist <- F.fileExist file
    when (not exist)
      $ die ("File not found: " ++ file)

    (hash, size) <- F.getHashAndSize file

    putStrLn "Logging..."
    loginResp <- login "" "" lang userAgent
    checkResponseStatus (L.status loginResp)

    let token = fromJust (L.token loginResp)
    putStrLn ("Logged: " ++ token)

    putStrLn "Searching..."
    searchResp <- search token [(S.SearchRequest file lang hash size)]
    checkResponseStatus (S.status searchResp)

    let searchData = S.result searchResp
    let count = length searchData

    when (count == 0)
      $ die "No subtitles found"

    putStrLn $ "Found " ++ show count ++ " results"
    i <- askForSub searchData

    when (i == "0")
      $ exit "Closing..."

    putStrLn "Downloading..."

    downResp <- download token [i]
    checkResponseStatus (D.status downResp)

    putStrLn "Saving..."
    F.saveSubtitle file $ F.decodeAnddecompress (D.data_ $ head (D.result downResp))

    putStrLn "Done."

askForSub :: [S.SearchSubResponse] -> IO String
askForSub list = do
                  let orderedList = sortBy sortSubtitlesGT list
                  let indexedList = zip ([1..] :: [Integer]) orderedList
                  putStrLn "Subtitles found: "
                  mapM_ (\(i, (S.SearchSubResponse is n r c)) -> putStrLn $ show i ++ "- "++ is ++ " " ++ n ++ "(" ++ r ++ "/" ++ c ++ ")" ) indexedList
                  putStr "Choose a subtitle from the list (0 to exit): "
                  n <- getLine
                  if validate n then
                    if (read n :: Integer) == 0 then
                      return "0"
                    else
                      case find (\(i, _) -> i == (read n)) indexedList of
                          Nothing -> askForSub list
                          Just (_, s) -> return (S.idSubtitleFile s)
                  else
                    askForSub list
                where
                  validate n =  (not . null) n && (all isDigit) n && (read n <= length list)

sortSubtitlesGT :: S.SearchSubResponse -> S.SearchSubResponse -> Ordering
sortSubtitlesGT (S.SearchSubResponse _ _ r1 c1) (S.SearchSubResponse _ _ r2 c2) =
                  case compare (c r1) (c r2) of
                    EQ -> compare (c c2) (c c1)
                    LT -> GT
                    GT -> LT
              where
                c n = (read n :: Double)

checkResponseStatus :: String -> IO ()
checkResponseStatus ('2':_) = return ()
checkResponseStatus s = die s

exit :: String -> IO ()
exit msg = do
  hPutStrLn stdout msg
  exitWith ExitSuccess

die :: String -> IO ()
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)
