module Hassub where

import Data.List
import Control.Monad
import Data.Char
import Data.Maybe

import OpenSubtitles
import Login
import Search
import Download
import File
import Options

userAgent = "myapp"

getSubtitle :: SubLanguageId -> String -> IO ()
getSubtitle lang file = do
    putStrLn $ "Searching for " ++ file ++ " in " ++ lang

    exist <- fileExist file
    when (not exist)
      $ die ("File not found: " ++ file)

    (hash, size) <- getHashAndSize file

    putStrLn "Logging..."
    (LoginResponse t s) <- login "" "" lang userAgent
    checkResponseStatus s

    let token = fromJust t
    putStrLn ("Logged: " ++ token)

    putStrLn "Searching..."
    (SearchResponse s d) <- search token [(SearchRequest file lang hash size)]
    checkResponseStatus s

    putStrLn $ "Found " ++ (show . length) d ++ " results"
    (SearchSubResponse i _ _ _) <- askForSub d
    putStrLn "Downloading..."

    (DownloadResponse s r) <- download token [i]
    checkResponseStatus s

    putStrLn "Saving..."
    saveSubtitle file $ decodeAnddecompress (data_ (head r))

    putStrLn "Done."


askForSub :: [SearchSubResponse] -> IO SearchSubResponse
askForSub list = do
                  let orderedList = sortBy sortSubtitlesGT list
                  let indexedList = zip ([1..] :: [Integer]) orderedList
                  putStrLn "Subtitles found: "
                  mapM_ (\(i, (SearchSubResponse is n r c)) -> putStrLn $ show i ++ "- "++ is ++ " " ++ n ++ "(" ++ r ++ "/" ++ c ++ ")" ) indexedList
                  putStr "Choose a subtitle from the list: "
                  n <- getLine
                  if validate n then
                    case find (\(i, _) -> i == (read n)) indexedList of
                        Nothing -> askForSub list
                        Just (_, s) -> return s
                   else
                      askForSub list
                where
                  validate n =  (not . null) n && (all isDigit) n && (read n > 0 && read n <= length list)

sortSubtitlesGT :: SearchSubResponse -> SearchSubResponse -> Ordering
sortSubtitlesGT (SearchSubResponse _ _ r1 c1) (SearchSubResponse _ _ r2 c2) =
                  case compare (c r1) (c r2) of
                    EQ -> compare (c c2) (c c1)
                    LT -> GT
                    GT -> LT
              where
                c n = (read n :: Double)

checkResponseStatus :: String -> IO ()
checkResponseStatus ('2':_) = return ()
checkResponseStatus s = die s
