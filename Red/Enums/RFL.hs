module Red.Enums.RFL where

import Debug.Trace

import Red.Enums.FromBinary

data RFLSectionType
    = RFLEnd
    | RFLStaticGeo
    | RFLGeoRegions
    | RFLLights
    | RFLCutsceneCameras
    | RFLAmbientSounds
    | RFLEvents
    | RFLMultiPlayerRespawns
    | RFLLevelProperties
    | RFLParticleEmitters
    | RFLGasRegions
    | RFLRoomEffects
    | RFLBoltEmitters
    | RFLTargets
    | RFLDecals
    | RFLPushRegions
    | RFLLightMaps
    | RFLMovers
    | RFLMovingGroups
    | RFLCutScenePathNodes
    | RFLTGAList
    | RFLVCMList
    | RFLMVFList
    | RFLV3DList
    | RFLVFXList
    | RFLEAXEffects
    | RFLNavPoints
    | RFLEntities
    | RFLItems
    | RFLClutters
    | RFLTriggers
    | RFLPlayerStart
    | RFLLevelInfo
    | RFLBrushes
    | RFLGroups
    | RFLUnknown
    deriving(Eq,Ord,Enum,Bounded,Show)

instance FromBinary32 RFLSectionType where
    fromBinary32 0x00000000 = RFLEnd
    fromBinary32 0x00000100 = RFLStaticGeo
    fromBinary32 0x00000200 = RFLGeoRegions
    fromBinary32 0x00000300 = RFLLights
    fromBinary32 0x00000400 = RFLCutsceneCameras
    fromBinary32 0x00000500 = RFLAmbientSounds
    fromBinary32 0x00000600 = RFLEvents
    fromBinary32 0x00000700 = RFLMultiPlayerRespawns
    fromBinary32 0x00000900 = RFLLevelProperties
    fromBinary32 0x00000A00 = RFLParticleEmitters
    fromBinary32 0x00000B00 = RFLGasRegions
    fromBinary32 0x00000C00 = RFLRoomEffects
    fromBinary32 0x00000E00 = RFLBoltEmitters
    fromBinary32 0x00000F00 = RFLTargets
    fromBinary32 0x00001000 = RFLDecals
    fromBinary32 0x00001100 = RFLPushRegions
    fromBinary32 0x00001200 = RFLLightMaps
    fromBinary32 0x00002000 = RFLMovers
    fromBinary32 0x00003000 = RFLMovingGroups
    fromBinary32 0x00005000 = RFLCutScenePathNodes
    fromBinary32 0x00007000 = RFLTGAList
    fromBinary32 0x00007001 = RFLVCMList
    fromBinary32 0x00007002 = RFLMVFList
    fromBinary32 0x00007003 = RFLV3DList
    fromBinary32 0x00007004 = RFLVFXList
    fromBinary32 0x00008000 = RFLEAXEffects
    fromBinary32 0x00020000 = RFLNavPoints
    fromBinary32 0x00030000 = RFLEntities
    fromBinary32 0x00040000 = RFLItems
    fromBinary32 0x00050000 = RFLClutters
    fromBinary32 0x00060000 = RFLTriggers
    fromBinary32 0x00070000 = RFLPlayerStart
    fromBinary32 0x01000000 = RFLLevelInfo
    fromBinary32 0x02000000 = RFLBrushes
    fromBinary32 0x03000000 = RFLGroups
    fromBinary32 _ = RFLUnknown

data RFLFaceFlags
    = RFLFFNone
    | RFLFFShowSky
    | RFLFFMirrored
    | RFLFFFullBright
    | RFLFFMask
    | RFLFFUnknown
    deriving (Eq,Ord,Enum,Bounded,Show)

instance FromBinary8 RFLFaceFlags where
    fromBinary8 0x00 = RFLFFNone
    fromBinary8 0x01 = RFLFFShowSky
    fromBinary8 0x02 = RFLFFMirrored
    fromBinary8 0x20 = RFLFFFullBright
    fromBinary8 0xEF = RFLFFMask
    fromBinary8 _ = RFLFFUnknown

data RFLBrushFlags
    = RFLBFNone
    | RFLBFPortal
    | RFLBFAir
    | RFLBFPortalAir
    | RFLBFDetail
    | RFLBFEmitStream
    | RFLBFUnknown
    deriving (Show)

instance FromBinary32 RFLBrushFlags where
    fromBinary32 0x00 = RFLBFNone
    fromBinary32 0x01 = RFLBFPortal
    fromBinary32 0x02 = RFLBFAir
    fromBinary32 0x03 = RFLBFPortalAir
    fromBinary32 0x04 = RFLBFDetail
    fromBinary32 0x10 = RFLBFEmitStream
    fromBinary32 m = trace ("Unknown flag: " ++ show m) RFLBFUnknown
