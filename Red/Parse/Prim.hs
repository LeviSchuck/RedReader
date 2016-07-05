module Red.Parse.Prim
    ( wordToFloat
    , wordToInt32
    , niceHex8
    , niceHex16
    , niceHex32
    , hexPrint
    )
where

import Data.Word
import Data.Int
import Numeric

import qualified Foreign as F
import qualified Data.ByteString as B

import System.IO.Unsafe (unsafePerformIO)

toTarget :: (F.Storable word, F.Storable target) => word -> target
toTarget word = unsafePerformIO $ F.alloca $ \buf -> do
    F.poke (F.castPtr buf) word
    F.peek buf

fromTarget :: (F.Storable word, F.Storable target) => target -> word
fromTarget float = unsafePerformIO $ F.alloca $ \buf -> do
    F.poke (F.castPtr buf) float
    F.peek buf

floatToWord :: Float -> F.Word32
floatToWord = fromTarget

wordToFloat :: F.Word32 -> Float
wordToFloat = toTarget

wordToInt32 :: F.Word32 -> Int
wordToInt32 w = fromIntegral res
    where
        res :: Int32
        res = toTarget w


niceHex8 :: Word8 -> String
niceHex8 v
    | v < 16    = '0' : ps v
    | otherwise =       ps v
    where ps = flip showHex ""

niceHex16 :: Word16 -> String
niceHex16 v
    | v < 16   = '0' : '0' : '0' : ps v
    | v < 256  = '0' : '0' :       ps v
    | v < 4096 = '0' :             ps v
    | otherwise = ps v
    where ps = flip showHex ""

niceHex32 :: Word32 -> String
niceHex32 v
    | v < 16    = '0' : '0' : '0' : '0' : '0' : '0' : '0' : ps v
    | v < 256   = '0' : '0' : '0' : '0' : '0' : '0' :       ps v
    | v < 4096  = '0' : '0' : '0' : '0' : '0' :             ps v
    | v < 65536 = '0' : '0' : '0' : '0' :                  ps v
    | v < 1048576 = '0' : '0' : '0' : ps v
    | v < 16777216 = '0' : '0' : ps v
    | v < 268435456 = '0' : ps v
    | otherwise = ps v
    where ps = flip showHex ""

hexPrint :: B.ByteString -> String
hexPrint = concat . map niceHex8 . B.unpack
