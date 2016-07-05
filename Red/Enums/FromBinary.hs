module Red.Enums.FromBinary where

import Data.Word

class FromBinary32 a where
    fromBinary32 :: Word32 -> a


class FromBinary8 a where
    fromBinary8 :: Word8 -> a

