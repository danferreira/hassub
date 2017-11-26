module Hassub (getSubtitles) where

import Data.List                  (find, sortBy)
import Control.Monad              (when)
import Data.Char                  (isDigit)
import Data.Maybe                 (fromJust)
import System.IO                  (hPutStrLn, stderr, stdout, hFlush)
import System.Exit                (ExitCode (ExitSuccess, ExitFailure), exitWith)

import OpenSubtitles.API
import qualified OpenSubtitles.Login as L
import qualified OpenSubtitles.Search as S
import qualified OpenSubtitles.Download as D
import qualified File as F
import Appearance

type SubLanguageId = String
type Filename = String
type SilentMode = Bool

userAgent :: String
userAgent = "myapp"

getAtt :: SubLanguageId -> Filename -> IO S.SearchRequest
getAtt dLang f = do
            exist <- F.fileExist f
            when (not exist)
              $ die (red ++ "File not found" ++ reset ++ ": " ++ f)

            (hash, size) <- F.getHashAndSize f
            return (S.SearchRequest f dLang hash size)

getSubtitles :: SubLanguageId -> SilentMode -> [Filename] -> IO ()
getSubtitles lang sm fs = do
    putStrLn "Searching for subtitles..."

    files <- mapM (getAtt lang) fs

    putStrLn "Logging in..."
    loginResp <- login (L.LoginRequest "" "" lang userAgent)
    checkResponseStatus (L.status loginResp)

    let token = fromJust (L.token loginResp)

    mapM_ (getSubtitle token sm) files

    putStrLn "\nLogging out..."
    logoutResp <- logout token

    putStrLn $ green ++ "All subtitles saved." ++ reset


getSubtitle :: Token -> SilentMode -> S.SearchRequest -> IO ()
getSubtitle token sm f = do
    putStrLn $ "\nSearching for: " ++ green ++ (S.filename f) ++ reset
    searchResp <- search token [f]
    checkResponseStatus (S.status searchResp)

    let searchData = S.result searchResp
        count = length searchData

    when (count == 0)
      $ exit (yellow ++ "No subtitles found")

    putStrLn $ green ++ "Found" ++ reset ++ ": " ++ show count ++ " result(s)"

    i <- askForSub sm searchData

    when (i == "0")
      $ exit (yellow ++ "No subtitles downloaded")

    putStrLn "Downloading..."

    downResp <- download token [i]
    checkResponseStatus (D.status downResp)

    F.saveSubtitle (S.filename f) $ F.decodeAnddecompress (D.data_ $ head (D.result downResp))
    putStrLn $ green ++ "Success" ++ reset ++ ": Subtitle downloaded."


askForSub :: SilentMode -> [S.SearchSubResponse] -> IO String
askForSub sm list = do
                    let orderedList = sortBy sortSubtitlesGT list

                    if (sm) then
                      return $ S.idSubtitleFile (head orderedList)
                    else
                      do
                      let indexedList = zip ([1..] :: [Integer]) orderedList

                      putStrLn $ "\n"++ bold ++ blue ++ (padRight "#" 5) ++ (padRight "Subtitle" 50) ++ "(Rate/Downloads)"++reset
                      mapM_ (\(i, (S.SearchSubResponse _ n r c)) ->
                                putStrLn $ cyan ++ (padRight (show i) 5) ++ reset ++ (padRight n 50) ++ cyan ++ "(" ++ r ++ "/" ++ c ++ ")" ++ reset) indexedList
                      putStr $ "\nChoose a subtitle from the list" ++ yellow ++ " (0 to cancel): " ++ reset
                      hFlush stdout
                      n <- getLine
                      if validate n then
                        if (read n :: Integer) == 0 then
                          return "0"
                        else
                          case find (\(i, _) -> i == (read n)) indexedList of
                              Nothing -> askForSub sm list
                              Just (_, s) -> return (S.idSubtitleFile s)
                      else
                        askForSub sm list
                where
                  padRight v n | length v >= n = padRight ((take (n - 4) v) ++ "...") n
                               | otherwise = take n $ v ++ repeat ' '
                  validate n =  (not . null) n && (all isDigit) n && (read n <= length list)

sortSubtitlesGT :: S.SearchSubResponse -> S.SearchSubResponse -> Ordering
sortSubtitlesGT (S.SearchSubResponse _ _ r1 c1) (S.SearchSubResponse _ _ r2 c2) =
                  case compare (c c1) (c c2) of
                    EQ -> compare (c r2) (c r1)
                    LT -> GT
                    GT -> LT
              where
                c n = (read n :: Double)

checkResponseStatus :: String -> IO ()
checkResponseStatus ('2':_) = return ()
checkResponseStatus s = die $ red ++ s ++ reset

exit :: String -> IO ()
exit msg = do
  hPutStrLn stdout $ msg ++ reset
  exitWith ExitSuccess

die :: String -> IO ()
die msg = do
  hPutStrLn stderr $ msg ++ reset
  exitWith (ExitFailure 1)
