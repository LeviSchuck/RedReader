{-# LANGUAGE OverloadedStrings #-}
module Red.Parse.Base where

import Debug.Trace
import Control.Monad(forever,replicateM,when)
import Control.Applicative(many)
import Data.Word
import Data.Char(digitToInt,ord,chr)


import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.Binary as AB
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Red.Binary.Base
import Red.Parse.Prim

char :: Char -> AP.Parser ()
char c = do
    _ <- AP.word8 (fromIntegral (ord c))
    return ()


parseFloat :: AP.Parser Float
parseFloat = do
    f32 <- AB.anyWord32le
    return (wordToFloat f32)

parseInt :: AP.Parser Int
parseInt = do
    i32 <- AB.anyWord32le
    return (wordToInt32 i32)

parseUV :: AP.Parser RFUV
parseUV = do
    u <- parseFloat
    v <- parseFloat
    return $ RFUV u v

parseV3 :: AP.Parser RFV3
parseV3 = do
    v1 <- parseFloat
    v2 <- parseFloat
    v3 <- parseFloat
    return $ RFV3 v1 v2 v3

parseV4 :: AP.Parser RFV4
parseV4 = do
    v1 <- parseFloat
    v2 <- parseFloat
    v3 <- parseFloat
    v4 <- parseFloat
    return $ RFV4 v1 v2 v3 v4

parseAABB :: AP.Parser RFAABB
parseAABB = do
    m1 <- parseV3
    m2 <- parseV3
    return $ RFAABB m1 m2

parseMatrix3 :: AP.Parser RFMatrix3
parseMatrix3 = do
    v1 <- parseV3
    v2 <- parseV3
    v3 <- parseV3
    return $ RFMatrix3 v1 v2 v3

parseRGB24 :: AP.Parser RFRGB24
parseRGB24 = do
    r <- AP.anyWord8
    g <- AP.anyWord8
    b <- AP.anyWord8
    return $ RFRGB24 r g b

parseRGBA32 :: AP.Parser RFRGBA32
parseRGBA32 = do
    r <- AP.anyWord8
    g <- AP.anyWord8
    b <- AP.anyWord8
    a <- AP.anyWord8
    return $ RFRGBA32 r g b a

parseBitmap24 :: AP.Parser RFBitmap24
parseBitmap24 = do
    w <- AB.anyWord32le
    h <- AB.anyWord32le
    bmp <- AP.take (fromIntegral (w * h * 3))
    return $ RFBitmap24 w h bmp

parseLengthedText :: AP.Parser B.ByteString
parseLengthedText = do
    len16 <- AB.anyWord16le
    let len = fromIntegral len16
    -- traceM ("Got length 0x" ++ niceHex16 len16)
    if len > 255
        then do
            traceM ("Length is " ++ show len)
            traceM "Panic!"
            return B.empty
        else do
            text <- AP.take len
            return text

parseVector :: Integral a => a -> AP.Parser x -> AP.Parser (V.Vector x)
parseVector i p = do
    xL <- replicateM (fromIntegral i) p
    return $ V.fromList xL

wrapAlign :: Word32 -> AP.Parser x -> AP.Parser x
wrapAlign align fun = do
    (bs, res) <- AP.match fun
    let l = fromIntegral $ B.length bs
        r = l `rem` align
    when (r /= 0) $ do
        _ <- AP.take (fromIntegral (align - r))
        return ()
    return res

parseBytes :: B.ByteString -> AP.Parser a -> a
parseBytes m fun = case AP.parse fun m of
    AP.Done _ res -> res
    AP.Fail _ _ e -> error e
    AP.Partial f -> case f B.empty of
        AP.Done _ res -> res
        AP.Fail _ _ e -> error e
        _ -> error "EOF EOF??"

ignoreWhiteSpace :: AP.Parser ()
ignoreWhiteSpace = do
    _ <- many $ AP.takeWhile1 (AP.inClass " \t\r")
    return ()

parseTextNumbers :: AP.Parser (Bool,Maybe Int, Maybe (Int,Int))
parseTextNumbers = do
    isPos <- AP.option True $ do
        char '-'
        return False

    let gn = do
        ns <- AP.takeWhile1 (AP.inClass "1234567890")
        let ns1 = map (digitToInt . chr . fromIntegral) (B.unpack ns)
        let ns2 = foldl (\x n -> x * 10 + n) 0 ns1
        return (ns2, B.length ns)

    a <- AP.option Nothing $ do
        (n, _) <- gn
        return $ Just n
    b <- AP.option Nothing $ do
        char '.'
        (n, c) <- gn
        return $ Just (n, c)
    return (isPos,a,b)

parseTextNumber :: AP.Parser (Maybe Int, Maybe Float)
parseTextNumber = do
    (p, n, d) <- parseTextNumbers
    let ci = if p then 1 else -1
        cf = if p then 1 else -1
        lh = case d of
            Just (dn, dc) -> Just ((fromIntegral dn) / (10.0 ** (fromIntegral dc)))
            Nothing -> Nothing
    case (n, lh) of
        -- Handle .450
        (Nothing, Just lh) -> return $ (Nothing, Just (cf * lh))
        (Just x, Just lh) -> return $ (Nothing, Just (cf * (fromIntegral x + lh)))
        (Just x, Nothing) -> return $ (Just (ci * x), Just (cf * (fromIntegral x)))
        (Nothing, Nothing) -> fail "Not a number"

parseTextNumberInt :: AP.Parser Int
parseTextNumberInt = do
    (n, _) <- parseTextNumber
    case n of
        Just x -> return x
        _ -> fail "Not an integer"


parseTextNumberFloat :: AP.Parser Float
parseTextNumberFloat = do
    (_, f) <- parseTextNumber
    case f of
        Nothing -> fail "Not a float"
        Just x -> return $ x

