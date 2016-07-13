module Red.Parse.VPP where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Word

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.Binary as AB
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Map as M

import Red.Binary.Base
import Red.Binary.VPP
import Red.Parse.Base

parseVPPHeader :: AP.Parser VPPHeader
parseVPPHeader = do
    sig <- AB.word32le vppSignature
    ver <- AB.word32le 0x1
    fc <- AB.anyWord32le
    when (fc > vppMaxFiles) $ fail "File count is greater than max on this VPP"
    archsize <- AB.anyWord32le
    return $ VPPHeader sig ver fc archsize

parseVPPEntry :: AP.Parser VPPEntry
parseVPPEntry = do
    name60 <- AP.take 60
    size <- AB.anyWord32le
    let name = B.takeWhile (/= 0) name60
    return $ VPPEntry name size

parseVPP :: AP.Parser VPP
parseVPP = do
    let wa = wrapAlign vppAlignment
    header <- wa parseVPPHeader
    let c = (vppHeaderFileCount header)
    traceM ("VPP File count: " ++ show c)
    fs <- wa $ parseVector c  parseVPPEntry
    -- traceM ("VPP Files:")
    {-forM_ fs $ \fh -> do
        let fn = vppEntryFileName fh
            fs = vppEntryFileSize fh
        traceM ("- " ++ show fn ++ " size " ++ show fs)
    -}
    bins <- forM fs $ \fh -> wa $ AP.take $ fromIntegral (vppEntryFileSize fh)
    let combo = V.zip fs bins
    let files = V.foldl (\a (f,d) -> M.insert (vppEntryFileName f) d a) M.empty combo
    return $ VPP header fs files
