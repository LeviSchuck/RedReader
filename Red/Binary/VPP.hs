module Red.Binary.VPP where

import Data.Word

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Map as M

import Red.Binary.Base

vppSignature :: Word32
vppSignature = 0x51890ACE

vppAlignment :: Word32
vppAlignment = 0x800

vppMaxFiles :: Word32
vppMaxFiles = 65536

data VPPHeader = VPPHeader
    { vppHeaderSignature :: Word32
    , vppHeaderVersion :: Word32
    , vppHeaderFileCount :: Word32
    , vppHeaderArchiveSize :: Word32
    } deriving (Show)

data VPPEntry = VPPEntry
    { vppEntryFileName :: B.ByteString
    , vppEntryFileSize :: Word32
    } deriving (Show)

data VPP = VPP
    { vppHeader :: VPPHeader
    , vppFiles :: V.Vector VPPEntry
    , vppFileMap :: M.Map B.ByteString B.ByteString
    } deriving (Show)
