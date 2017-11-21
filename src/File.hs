module File (getHashAndSize, decodeAnddecompress, saveSubtitle, fileExist) where

import Control.Exception
import System.IO(openBinaryFile,hClose,hFileSize,hSeek,IOMode(ReadMode),SeekMode(AbsoluteSeek,SeekFromEnd))
import qualified Data.ByteString.Lazy as L(hGet,unpack)
import Data.Binary.Get(runGet,getWord64le)
import Data.Binary.Put(runPut,putWord64le)
import Data.Word(Word64)
import Control.Monad(foldM)
import Data.Bits.Utils(w82s)
import Data.Hex(hex)
import System.Directory
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Codec.Binary.Base64.String as Base64

import Options

fileExist :: FilePath -> IO Bool
fileExist = doesFileExist

shortsum :: FilePath -> IO (Word64, Double)
shortsum filename = bracket (openBinaryFile filename ReadMode) hClose $ \h -> do
  fs <- hFileSize h
  hSeek h AbsoluteSeek 0 ; begin <- L.hGet h chunksize
  hSeek h SeekFromEnd (-(toInteger chunksize)) ; end <- L.hGet h chunksize
  return $ (,) ((flip runGet $ begin) $ chunksum $ (flip runGet $ end) (chunksum . fromInteger $ fs)) (fromInteger fs)
  where
    chunksize = 0x10000
    chunksum n = foldM (\a _ -> getWord64le >>= return . (+a)) n [1..(chunksize`div`8)]

getHashAndSize :: String -> IO (String, Double)
getHashAndSize fn = do
  (w64, fs) <- shortsum fn
  return $ (,) (hex $ w82s $ reverse (L.unpack $ runPut $ putWord64le w64)) fs

decodeAnddecompress = decompress . LBS8.pack . Base64.decode

saveSubtitle :: String -> LBS8.ByteString -> IO ()
saveSubtitle name content = LBS8.writeFile (getSubName) content
          where
            getSubName = (reverse . dropWhile (/= '.') . reverse) name ++ "srt"
