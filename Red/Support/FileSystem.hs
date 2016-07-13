module Red.Support.FileSystem where

import Control.Monad

import Debug.Trace

import qualified Data.Map as M
import qualified Data.ByteString as B

import Red.Binary.Base
import Red.Binary.RFL
import Red.Binary.VPP
import Red.Parse.Base
import Red.Parse.RFL
import Red.Parse.VPP


data VPPData = VPPData
    { vppsLoaded :: M.Map String VPP
    , filesLoaded :: M.Map B.ByteString String
    }

emptyVPPData = VPPData M.empty M.empty

addVPP :: VPPData -> String -> IO VPPData
addVPP vppdata path = do
    vppFile <- B.readFile path
    let vpp = parseBytes vppFile parseVPP
        nextLoaded = M.insert path vpp (vppsLoaded vppdata)
        loaded = filesLoaded vppdata
        files = M.keys (vppFileMap vpp)
        nextFiles = foldl add loaded files
    return $ vppdata { vppsLoaded = nextLoaded, filesLoaded = nextFiles}
    where
        add loaded name = M.insert name path loaded

loadFile :: VPPData -> B.ByteString -> IO (Maybe B.ByteString)
loadFile vppData file = do
    -- traceM ("Loading file: " ++ show file)
    let fileLower = toLowerBytes file
    case M.lookup fileLower (filesLoaded vppData) of
        Nothing -> do
            traceM ("Not listed in VPPs " ++ show file)
            -- TODO: fallback to local files
            return Nothing
        Just x -> case M.lookup x (vppsLoaded vppData) of
            Nothing -> fail "Known to be in VPP but could not find?"
            Just vpp -> return (getFile vpp file)

loadMap :: VPPData -> B.ByteString -> IO (Maybe RFL)
loadMap vppData file = do
    bytes <- loadFile vppData file
    case bytes of
        Nothing -> return Nothing
        Just rflFile -> return (Just (parseBytes rflFile parseRFL))
