{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.Foldable

import qualified Data.ByteString as B

import Red.Parse.Base
import Red.Parse.RFL
import Red.Parse.VPP
import Red.Parse.TBL

parseMap name = do
    cont <- B.readFile name
    let rfl = parseBytes cont parseRFL
    putStrLn . show . length . show $ rfl
    putStrLn "Parsed successfully!"
    putStrLn $ show (rflHeader rfl)
    forM_ (rflSections rfl) $ \(sh, s) -> case s of
        RFLSectUnknown _ -> putStrLn $ show sh
        _ -> return ()

parseArchive name = do
    cont <- B.readFile name
    let vpp = parseBytes cont parseVPP
    putStrLn . show . length . show $ vpp
    putStrLn "Parsed successfully!"

parseTable name = do
    cont <- B.readFile name
    let TBLFile tbl = parseBytes cont parseTBL
    forM_ tbl $ \ln -> do
        putStrLn $ show ln
    putStrLn "Parsed successfully!"

mt =
    [ "dm01.rfl"
    , "dm02.rfl"
    , "dm03.rfl"
    , "dm04.rfl"
    , "dm05.rfl"
    , "dm06.rfl"
    , "dm07.rfl"
    , "dm08.rfl"
    , "dm09.rfl"
    , "dm10.rfl"
    , "dm12.rfl"
    , "dm13.rfl"
    , "dm14.rfl"
    , "dm15.rfl"
    , "dm16.rfl"
    , "dm17.rfl"
    , "dm19.rfl"
    , "dm20.rfl"
    , "ctf01.rfl"
    , "ctf02.rfl"
    , "ctf03.rfl"
    , "ctf04.rfl"
    , "ctf05.rfl"
    , "ctf06.rfl"
    , "ctf07.rfl"
    , "pdm01.rfl"
    , "pdm02.rfl"
    , "pdm03.rfl"
    , "pctf01.rfl"
    , "pctf02.rfl"
    , "pctf03.rfl"
    ]
sp =
    [ "L10S1.rfl"
    , "L10S2.rfl"
    , "L10S3.rfl"
    , "L10S4.rfl"
    , "L11S1.rfl"
    , "L11S2.rfl"
    , "L11S3.rfl"
    , "L12S1.rfl"
    , "L12S2.rfl"
    , "L13S1.rfl"
    , "L13S3.rfl"
    , "L14S1.rfl"
    , "L14S2.rfl"
    , "L14S3.rfl"
    , "L15S1.rfl"
    , "L15S2.rfl"
    , "L15S4.rfl"
    , "L17S1.rfl"
    , "L17S2.rfl"
    , "L17S3.rfl"
    , "L17S4.rfl"
    , "L18S1.rfl"
    , "L18S2.rfl"
    , "L18S3.rfl"
    , "L19S1.rfl"
    , "L19S2A.rfl"
    , "L19S2B.rfl"
    , "L19S3.rfl"
    , "L1S1.rfl"
    , "L1S2.rfl"
    , "L1S3.rfl"
    , "L20S1.rfl"
    , "L20S2.rfl"
    , "L20S3.rfl"
    , "L2S1.rfl"
    , "L2S2a.rfl"
    , "L2S3.rfl"
    , "L3S1.rfl"
    , "L3S2.rfl"
    , "L3S3.rfl"
    , "L3S4.rfl"
    , "L4S1a.rfl"
    , "L4S1b.rfl"
    , "L4S2.rfl"
    , "L4S3.rfl"
    , "L4S4.rfl"
    , "L4S5.rfl"
    , "L5S1.rfl"
    , "L5S2.rfl"
    , "L5S3.rfl"
    , "L5S4.rfl"
    , "L6S1.rfl"
    , "L6S2.rfl"
    , "L6S3.rfl"
    , "L7S1.rfl"
    , "L7S2.rfl"
    , "L7S3.rfl"
    , "L7S4.rfl"
    , "L8S1.rfl"
    , "L8S2.rfl"
    , "L8S3.rfl"
    , "L8S4.rfl"
    , "L9S1.rfl"
    , "L9S2.rfl"
    , "L9S3.rfl"
    , "L9S4.rfl"
    , "train01.rfl"
    , "train02.rfl"
    ]

mts = map (\x -> "/Volumes/Public/maps/MT/" ++ x) mt
sps = map (\x -> "/Volumes/Public/maps/RF/" ++ x) sp

testAll :: IO ()
testAll = do
    let both = mts ++ sps
    r <- foldrM (\f c -> do
        putStrLn $ "Gonna parse " ++ f
        cont <- B.readFile f
        let !rfl = parseBytes cont parseRFL
        return (c + 1)) 0 both
    putStrLn $ "Total length: " ++ show r

main :: IO ()
main = do
    putStrLn "Ohai"
    testAll
    parseArchive "test1.vpp"
    parseTable "test.tbl"
