module Hassub (getSubtitles) where

import           Control.Monad          (unless, when)
import           Data.Char              (isDigit)
import           Data.List              (find, sortBy)
import           Data.Maybe             (fromJust)
import           System.Exit            (ExitCode (ExitFailure), exitSuccess,
                                         exitWith)
import           System.IO              (hFlush, hPutStrLn, stderr, stdout)

import           Appearance
import qualified File                   as F
import qualified OpenSubtitles.API      as A
import qualified OpenSubtitles.Download as D
import qualified OpenSubtitles.Login    as L
import qualified OpenSubtitles.Search   as S

type SubLanguageId = String
type Filename = String
type SilentMode = Bool
type Username = String
type Password = String

userAgent :: String
userAgent = "TemporaryUserAgent"

getAtt :: SubLanguageId -> Filename -> IO S.SearchRequest
getAtt dLang f = do
            exist <- F.fileExist f
            unless exist
              $ die (red "File not found" ++ ": " ++ f)

            (hash, size) <- F.getHashAndSize f
            return (S.SearchRequest f dLang hash size)

getSubtitles :: SubLanguageId -> Username -> Password -> SilentMode -> [Filename] -> IO ()
getSubtitles lang user pass sm fs = do
    putStrLn $ "Searching for subtitles using " ++ green lang ++ "..."

    files <- mapM (getAtt lang) fs

    let u = if null user then "anonymously" else  "as " ++ user

    putStrLn $ "Logging in " ++ u
    loginResp <- A.login (L.LoginRequest user pass lang userAgent)
    checkResponseStatus (L.status loginResp)

    let token = fromJust (L.token loginResp)

    mapM_ (getSubtitle token sm) files

    putStrLn "\nLogging out..."
    _ <- A.logout token

    putStrLn $ green "All subtitles saved."


getSubtitle :: A.Token -> SilentMode -> S.SearchRequest -> IO ()
getSubtitle token sm f = do
    putStrLn $ "\nSearching for: " ++ green (S.filename f)
    searchResp <- A.search token [f]
    checkResponseStatus (S.status searchResp)

    let searchData = S.result searchResp
        count = length searchData

    when (count == 0)
      $ exit (yellow "No subtitles found")

    putStrLn $ green "Found" ++ ": " ++ show count ++ " result(s)"

    i <- askForSub sm searchData

    when (i == "0")
      $ exit (yellow "No subtitles downloaded")

    putStrLn "Downloading..."

    downResp <- A.download token [i]
    checkResponseStatus (D.status downResp)

    F.saveSubtitle (S.filename f) $ F.decodeAnddecompress (D.data_ $ head (D.result downResp))
    putStrLn $ green "Success" ++ ": Subtitle downloaded."


askForSub :: SilentMode -> [S.SearchSubResponse] -> IO String
askForSub sm list = do
                    let orderedList = sortBy sortSubtitlesGT list

                    if sm then
                      return $ S.idSubtitleFile (head orderedList)
                    else
                      do
                      let indexedList = zip ([1..] :: [Integer]) orderedList

                      putStrLn $ "\n" ++ (bold . blue) (padRight 5 "#" ++ padRight 50 "Subtitle" ++ "(Rate/Downloads)")
                      mapM_ (\(i, S.SearchSubResponse _ n r c) ->
                                putStrLn $ cyan (padRight 5 (show i)) ++ padRight 50 n ++ cyan ("(" ++ r ++ "/" ++ c ++ ")")) indexedList
                      putStr $ "\nChoose a subtitle from the list" ++ yellow " (0 to cancel): "
                      hFlush stdout
                      n <- getLine
                      if validate n then
                        if (read n :: Integer) == 0 then
                          return "0"
                        else
                          case find (\(i, _) -> i == read n) indexedList of
                              Nothing     -> askForSub sm list
                              Just (_, s) -> return (S.idSubtitleFile s)
                      else
                        askForSub sm list
                where
                  padRight n v | length v >= n = padRight n (take (n - 4) v ++ "...")
                               | otherwise = take n $ v ++ repeat ' '
                  validate n =  (not . null) n && all isDigit n && (read n <= length list)

sortSubtitlesGT :: S.SearchSubResponse -> S.SearchSubResponse -> Ordering
sortSubtitlesGT (S.SearchSubResponse _ _ r1 c1) (S.SearchSubResponse _ _ r2 c2) =
                  case compare (c c1) (c c2) of
                    EQ -> compare (c r2) (c r1)
                    LT -> GT
                    GT -> LT
              where
                c n = read n :: Double

checkResponseStatus :: String -> IO ()
checkResponseStatus ('2':_) = return ()
checkResponseStatus s       = die $ red s

exit :: String -> IO ()
exit msg = do
  putStrLn msg
  exitSuccess

die :: String -> IO ()
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)
