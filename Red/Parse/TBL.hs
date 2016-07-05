{-# LANGUAGE OverloadedStrings #-}
module Red.Parse.TBL where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Word

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.Binary as AB
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Red.Binary.Base

import Red.Parse.Base
import Red.Parse.Prim

data TBLElement
    = TBLElementHeader B.ByteString
    | TBLElementKey B.ByteString
    | TBLElementSubKey B.ByteString
    | TBLElementText B.ByteString
    | TBLElementXStr Int B.ByteString
    | TBLElementV3 RFV3
    | TBLElementBool Bool
    | TBLElementIntVec (V.Vector Int)
    | TBLElementListStr (V.Vector B.ByteString)
    | TBLElementListNum (V.Vector Float)
    | TBLElementFloat Float
    | TBLElementInt Int
    | TBLElementIgnore
    deriving (Show)

type TBLLine = V.Vector TBLElement

data TBLFile = TBLFile (V.Vector TBLLine) deriving (Show)

parseTBL :: AP.Parser TBLFile
parseTBL = do
    let lf x = AP.sepBy x (char '\n')
        header = do
            char '#'
            text <- AP.takeWhile (AP.notInClass "\r\n")
            return $ TBLElementHeader text
        comment = do
            AP.string "//"
            _ <- AP.takeWhile (AP.notInClass "\r\n")
            return TBLElementIgnore
        key = do
            char '$'
            name <- AP.takeWhile1 (AP.notInClass ":\r\n")
            char ':'
            return $ TBLElementKey name
        subkey = do
            char '+'
            name <- AP.takeWhile1 (AP.notInClass ":\r\n")
            char ':'
            return $ TBLElementSubKey name
        str = do
            char '"'
            text <- AP.takeWhile (AP.notInClass "\"\r\n")
            char '"'
            return text
        plainstr = do
            text <- str
            return $ TBLElementText text
        xstr = do
            AP.string "XSTR("
            i <- parseTextNumberInt
            char ','
            ignoreWhiteSpace
            text <- str
            char ')'
            return $ TBLElementXStr i text
        boolTrue = do
            _ <- (AP.string "true") <|> (AP.string "yes")
            return True
        boolFalse = do
            _ <- (AP.string "false") <|> (AP.string "no")
            return False
        bool = do
            tf <- AP.choice [boolTrue, boolFalse]
            return $ TBLElementBool tf
        floatElem = do
            fl <- parseTextNumberFloat
            return $ TBLElementFloat fl
        intElem = do
            i <- parseTextNumberInt
            return $ TBLElementInt
        num = do
            nm <- parseTextNumber
            case nm of
                (Nothing, Just x) -> return $ TBLElementFloat x
                (Just x, Nothing) -> return $ TBLElementInt x
                (_, Just x) -> return $ TBLElementFloat x
                _ -> fail "Not a number"
        list = do
            char '('
            let strs = do
                    vs <- many $ do
                        ignoreWhiteSpace
                        s <- str
                        ignoreWhiteSpace
                        return s
                    return $ TBLElementListStr (V.fromList vs)
                nums = do
                    vn <- many $ do
                        ignoreWhiteSpace
                        n <- parseTextNumberFloat
                        ignoreWhiteSpace
                        return n
                    return $ TBLElementListNum (V.fromList vn)
            v <- (AP.try strs) <|> nums
            char ')'
            return $ v
        intlist = do
            char '{'
            ints <- AP.sepBy1 parseTextNumberInt (char ',')
            char '}'
            return $ TBLElementIntVec (V.fromList ints)
        vec3 = do
            char '<'
            ignoreWhiteSpace
            v1 <- parseTextNumberFloat
            ignoreWhiteSpace
            char ','
            ignoreWhiteSpace
            v2 <- parseTextNumberFloat
            ignoreWhiteSpace
            char ','
            ignoreWhiteSpace
            v3 <- parseTextNumberFloat
            ignoreWhiteSpace
            char '>'
            return $ TBLElementV3 (RFV3 v1 v2 v3)
        rest = do
            text <- AP.takeWhile1 (AP.notInClass "\r\n")
            return $ TBLElementText text

    ls <- lf $ do
        ignoreWhiteSpace
        els <- many $ do
            el <- header 
                <|> key 
                <|> subkey
                <|> comment
                <|> plainstr
                <|> xstr
                <|> vec3
                <|> list
                <|> intlist
                <|> num
                <|> bool
                <|> rest
            ignoreWhiteSpace
            return el
        return $ V.fromList els
    AP.endOfInput
    return $ TBLFile (V.fromList ls)
