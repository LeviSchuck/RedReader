module Red.Binary.Base where

import Data.Word

import qualified Data.ByteString as B


data RFV3 = RFV3
    { rfV3x :: Float
    , rfV3y :: Float
    , rfV3z :: Float
    } deriving (Eq,Ord, Show)

data RFV4 = RFV4
    { rfV4x :: Float
    , rfV4y :: Float
    , rfV4z :: Float
    , rfV4w :: Float
    } deriving (Eq,Ord, Show)

data RFMatrix3 = RFMatrix3
    { rfM31 :: RFV3
    , rfM32 :: RFV3
    , rfM33 :: RFV3
    } deriving (Eq,Ord,Show)

data RFAABB = RFAABB
    { rfaabbMin :: RFV3
    , rfaabbMax :: RFV3
    } deriving (Eq,Ord,Show)

data RFUV = RFUV
    { rflu :: Float
    , rflv :: Float
    } deriving (Eq,Ord,Show)

data RFRGB24 = RFRGB24
    { rf24R :: Word8
    , rf24G :: Word8
    , rf24B :: Word8
    } deriving (Eq,Ord,Show)

data RFRGBA32 = RFRGBA32
    { rf32R :: Word8
    , rf32G :: Word8
    , rf32B :: Word8
    , rf32A :: Word8
    } deriving (Eq,Ord,Show)

data RFBitmap24 = RFBitmap24
    { rf24Width :: Word32
    , rf24Height :: Word32
    , rf24Arr :: B.ByteString
    } deriving (Eq,Ord,Show)
