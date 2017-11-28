{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad         (unless, when)
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
        unless (lang `elem` supportedLangs) $ putStrLn ("Unknown language: "++ lang ++". Use ISO 639-2 codes") >> exitSuccess

        let silent = isPresent args (shortOption 's')

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

supportedLangs :: [String]
supportedLangs = ["aar", "abk", "ace", "ach", "ada", "ady", "afa", "afh", "afr", "ain",
                  "aka", "akk", "alb", "ale", "alg", "alt", "amh", "ang", "apa", "ara",
                  "arc", "arg", "arm", "arn", "arp", "art", "arw", "asm", "ast", "ath",
                  "aus", "ava", "ave", "awa", "aym", "aze", "bad", "bai", "bak", "bal",
                  "bam", "ban", "baq", "bas", "bat", "bej", "bel", "bem", "ben", "ber",
                  "bho", "bih", "bik", "bin", "bis", "bla", "bnt", "bos", "bra", "bre",
                  "btk", "bua", "bug", "bul", "bur", "byn", "cad", "cai", "car", "cat",
                  "cau", "ceb", "cel", "cha", "chb", "che", "chg", "chi", "chk", "chm",
                  "chn", "cho", "chp", "chr", "chu", "chv", "chy", "cmc", "cop", "cor",
                  "cos", "cpe", "and", "cpf", "and", "cpp", "and", "cre", "crh", "crp",
                  "and", "csb", "cus", "cze", "dak", "dan", "dar", "day", "del", "den",
                  "dgr", "din", "div", "doi", "dra", "dua", "dum", "dut", "dyu", "dzo",
                  "efi", "egy", "eka", "elx", "eng", "enm", "epo", "est", "ewe", "ewo",
                  "fan", "fao", "fat", "fij", "fil", "fin", "fiu", "fon", "fre", "frm",
                  "fro", "fry", "ful", "fur", "gaa", "gay", "gba", "gem", "geo", "ger",
                  "gez", "gil", "gla", "gle", "glg", "glv", "gmh", "goh", "gon", "gor",
                  "got", "grb", "grc", "ell", "grn", "guj", "gwi", "hai", "hat", "hau",
                  "haw", "heb", "her", "hil", "him", "hin", "hit", "hmn", "hmo", "hrv",
                  "hun", "hup", "iba", "ibo", "ice", "ido", "iii", "ijo", "iku", "ile",
                  "ilo", "ina", "inc", "ind", "ine", "inh", "ipk", "ira", "iro", "ita",
                  "jav", "jpn", "jpr", "jrb", "kaa", "kab", "kac", "kal", "kam", "kan",
                  "kar", "kas", "kau", "kaw", "kaz", "kbd", "kha", "khi", "khm", "kho",
                  "kik", "kin", "kir", "kmb", "kok", "kom", "kon", "kor", "kos", "kpe",
                  "krc", "kro", "kru", "kua", "kum", "kur", "kut", "lad", "lah", "lam",
                  "lao", "lat", "lav", "lez", "lim", "lin", "lit", "lol", "loz", "ltz",
                  "lua", "lub", "lug", "lui", "lun", "luo", "and", "lus", "mac", "mad",
                  "mag", "mah", "mai", "mak", "mal", "man", "mao", "map", "mar", "mas",
                  "may", "mdf", "mdr", "men", "mga", "mic", "min", "mis", "mkh", "mlg",
                  "mlt", "mnc", "mni", "mno", "moh", "mol", "mon", "mos", "mwl", "mul",
                  "mun", "mus", "mwr", "myn", "myv", "nah", "nai", "nap", "nau", "nav",
                  "nbl", "nde", "ndo", "nds", "nep", "new", "nia", "nic", "niu", "nno",
                  "nob", "nog", "non", "nor", "nso", "nub", "nwc", "nya", "nym", "nyn",
                  "nyo", "nzi", "oci", "oji", "ori", "orm", "osa", "oss", "ota", "oto",
                  "paa", "pag", "pal", "pam", "pan", "pap", "pau", "peo", "per", "phi",
                  "phn", "pli", "pol", "pon", "por", "pra", "pro", "pus", "que", "raj",
                  "rap", "rar", "roa", "roh", "rom", "run", "rup", "rus", "sad", "sag",
                  "sah", "sai", "sal", "sam", "san", "sas", "sat", "scc", "scn", "sco",
                  "sel", "sem", "sga", "sgn", "shn", "sid", "sin", "sio", "sit", "sla",
                  "slo", "slv", "sma", "sme", "smi", "smj", "smn", "smo", "sms", "sna",
                  "snd", "snk", "sog", "som", "son", "sot", "spa", "srd", "srr", "ssa",
                  "ssw", "suk", "sun", "sus", "sux", "swa", "swe", "syr", "tah", "tai",
                  "tam", "tat", "tel", "tem", "ter", "tet", "tgk", "tgl", "tha", "tib",
                  "tig", "tir", "tiv", "tkl", "tlh", "tli", "tmh", "tog", "ton", "tpi",
                  "tsi", "tsn", "tso", "tuk", "tum", "tup", "tur", "tut", "tvl", "twi",
                  "tyv", "udm", "uga", "uig", "ukr", "umb", "und", "urd", "uzb", "vai",
                  "ven", "vie", "vol", "vot", "wak", "wal", "war", "was", "wel", "wen",
                  "wln", "wol", "xal", "xho", "yao", "yap", "yid", "yor", "ypk", "zap",
                  "zen", "zha", "znd", "zul", "zun", "rum", "pob", "mne", "zht", "zhe",
                  "pom", "ext"]
