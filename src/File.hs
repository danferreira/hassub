{-# LANGUAGE OverloadedStrings #-}

module File (getHashAndSize, decodeAnddecompress, saveSubtitle, fileExist, getFilesInDirectory) where

import           Codec.Compression.GZip      (decompress)
import           Control.Monad               (foldM)
import           Data.Binary.Get             (getWord64le, runGet)
import           Data.Binary.Put             (putWord64le, runPut)
import           Data.Bits.Utils             (w82s)
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy        as L (hGet, unpack)
import qualified Data.ByteString.Lazy.Char8  as LBS8
import           Data.Hex                    (hex)
import           Data.Word                   (Word64)
import           System.Directory
import           System.IO                   (IOMode (ReadMode), SeekMode (AbsoluteSeek, SeekFromEnd),
                                              hFileSize, hSeek, withBinaryFile)
import qualified Utils                       as U

fileExist :: FilePath -> IO Bool
fileExist = doesFileExist

shortsum :: FilePath -> IO (Word64, Double)
shortsum filename = withBinaryFile filename ReadMode $ \h -> do
  fs <- hFileSize h
  hSeek h AbsoluteSeek 0 ; begin <- L.hGet h chunksize
  hSeek h SeekFromEnd (-(toInteger chunksize)) ; end <- L.hGet h chunksize
  return $ (,) ((`runGet` begin) $ chunksum $ (`runGet` end) (chunksum . fromInteger $ fs)) (fromInteger fs)
  where
    chunksize = 0x10000
    chunksum n = foldM (\a _ -> (+a) <$> getWord64le) n [1..(chunksize`div`8)]

getHashAndSize :: String -> IO (String, Double)
getHashAndSize fn = do
  (w64, fs) <- shortsum fn
  return $ (,) (hex $ w82s $ reverse (L.unpack $ runPut $ putWord64le w64)) fs

decodeAnddecompress :: String -> LBS8.ByteString
decodeAnddecompress = decompress . decode . LBS8.pack

decode :: LBS8.ByteString -> LBS8.ByteString
decode bs = case LB64.decode bs of
              (Left err) -> error err
              (Right x)  -> x

saveSubtitle :: String -> LBS8.ByteString -> IO ()
saveSubtitle name = LBS8.writeFile getSubName
          where
            getSubName = (reverse . dropWhile (/= '.') . reverse) name ++ "srt"

getFilesInDirectory :: IO [String]
getFilesInDirectory = filter (\f -> getExtension f `elem` U.supportedExt) <$> listDirectory "."

getExtension :: String -> String
getExtension = reverse . takeWhile (/= '.') . reverse
