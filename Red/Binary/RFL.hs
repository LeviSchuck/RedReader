module Red.Binary.RFL where

import Data.Word

import qualified Data.ByteString as B
import qualified Data.Vector as V

import Red.Enums.RFL
import Red.Binary.Base

rflSignature :: Word32
rflSignature = 0xD4BADA55

data RFLHeader = RFLHeader
    { rflVersion :: Word32
    , rflUnixTimestamp :: Word32
    , rflOffsetPlayerStart :: Word32
    , rflOffsetLevelInfo :: Word32
    , rflSectionsCount :: Word32
    , rflUnknown :: Word32
    , rflLevelName :: B.ByteString
    , rflModName :: B.ByteString
    } deriving (Show)

data RFLSectionHeader = RFLSectionHeader
    { rflSectionType :: RFLSectionType
    , rflSectionLength :: Word32
    } deriving (Show)

data RFLSpawn = RFLSpawn
    { rflSpawnTeamID :: Word32
    , rflSpawnBlueTeam :: Word8
    , rflSpawnRedTeam :: Word8
    , rflSpawnBot :: Word8
    } deriving (Show)

data RFLEAXEffect = RFLEAXEffect
    { rflEAXEffectType :: B.ByteString
    , rflEAXEffectUnknownArr1 :: B.ByteString
    , rflEAXEffectUnknownName1 :: B.ByteString
    , rflEAXEffectParams :: [Float] -- 12 floats
    , rflEAXEffectUnknownName2 :: B.ByteString
    , rflEAXEffectUnknownArr2 :: B.ByteString
    } deriving (Show)

data RFLRoomEffect
    = RFLRoomEffect1
    { rflRoomEffectUnknownArr1 :: B.ByteString
    , rflRoomEffectText1 :: B.ByteString
    , rflRoomEffectUnknownArr2 :: B.ByteString
    , rflRoomEffectText2 :: B.ByteString
    , rflRoomEffectUnknownArr3 :: B.ByteString
    }
    | RFLRoomEffect2
    { rflRoomEffectUnknownArr1 :: B.ByteString
    , rflRoomEffectText1 :: B.ByteString
    , rflRoomEffectUnknownArr2 :: B.ByteString
    , rflRoomEffectText2 :: B.ByteString
    , rflRoomEffectUnknownArr3 :: B.ByteString
    , rflRoomEffectText3 :: B.ByteString
    , rflRoomEffectUnknownArr4 :: B.ByteString
    }
    | RFLRoomEffect3
    { rflRoomEffectUnknownArr1 :: B.ByteString
    , rflRoomEffectText1 :: B.ByteString
    , rflRoomEffectUnknownArr2 :: B.ByteString
    , rflRoomEffectText2 :: B.ByteString
    , rflRoomEffectUnknownArr3 :: B.ByteString
    }
    | RFLRoomEffect4
    { rflRoomEffectUnknownArr1 :: B.ByteString
    , rflRoomEffectText1 :: B.ByteString
    , rflRoomEffectUnknownArr2 :: B.ByteString
    , rflRoomEffectText2 :: B.ByteString
    , rflRoomEffectUnknownArr3 :: B.ByteString
    }
    deriving (Show)

data RFLRoom = RFLRoom
    { rflRoomID :: Word32
    , rflRoomAABB :: RFAABB
    , rflRoomIsSkyRoom :: Word8
    , rflRoomIsCold :: Word8
    , rflRoomIsOutside :: Word8
    , rflRoomIsAirlock :: Word8
    , rflRoomIsLiquid :: Word8
    , rflRoomIsAmbientLight :: Word8
    , rflRoomIsSubroom :: Word8
    , rflRoomUnknown :: Word8
    , rflRoomLife :: Float
    , rflRoomEaxEffect :: B.ByteString
    , rflRoomProperties :: [RFLRoomProperties]
    }
    | RFLRoomDummy B.ByteString
    deriving (Show)

data RFLRoomProperties
    = RFLRoomPropertiesLiquid
        { rflRoomLiquidDepth :: Float
        , rflRoomLiquidColor :: Word32
        , rflRoomLiquidSurfaceTexture :: B.ByteString
        , rflRoomLiquidVisibility :: Float
        , rflRoomLiquidType :: Word32
        , rflRoomLiquidAlpha :: Word32
        , rflRoomLiquidUnknown :: B.ByteString
        , rflRoomLiquidWaveform :: Float
        , rflRoomLiquidSurfaceUV :: RFUV
        }
    | RFLRoomPropertiesAmbient
        { rflRoomAmbientColor :: Word32
        }
    | RFLRoomPropertiesNone
    deriving (Show)

data RFLVertex = RFLVertex
    { rflVIndex :: Word32
    , rflTexUV :: RFUV
    , rflLightUV :: Maybe RFUV
    } deriving (Show)

data RFLFace = RFLFace
    { rflFaceNormal :: RFV4
    , rflFaceTextureID :: Word32
    , rflFaceUnknown1 :: Word32
    , rflFaceUnknown2 :: Word32 -- Looks like a face ID
    , rflFaceUnknownArr8 :: B.ByteString
    , rflFaceUnknown3 :: Word32
    , rflFaceFlags :: RFLFaceFlags
    , rflFaceLightRes :: Word8 -- Should be an enum
    , rflFaceUnknownArr6 :: B.ByteString
    , rflFaceRoomIndex :: Word32
    , rflFaceVertexCount :: Word32
    , rflFaceVertexes :: V.Vector RFLVertex
    } deriving (Show)

data RFLFaceScroll = RFLFaceScroll
    { rflFaceScrollID :: Word32
    , rflFaceScrollUV :: RFUV
    } deriving (Show)

data RFLSectionRoomsMeshLink = RFLSectionRoomsMeshLink
    { rflSectRoomMeshID :: Word32
    , rflSectRoomMeshLinkCount :: Word32
    , rflSectRoomMeshLinks :: V.Vector Word32
    } deriving (Show)

data RFLSectionRoomsLightmap = RFLSectionRoomsLightmap
    { rflSectRoomFaceLightmapID :: Word32
    , rflSectRoomFaceLightmapUnknownArr :: B.ByteString
    , rflSectRoomFaceFaceID :: Word32
    } deriving (Show)

data RFLSectionRooms = RFLSectionRooms
    { rflSectRoomUnknownArr2A :: B.ByteString
    , rflSectRoomTextureCount :: Word32
    , rflSectRoomTextures :: V.Vector B.ByteString
    , rflSectRoomScrollCount :: Word32
    , rflSectRoomScrolls :: V.Vector RFLFaceScroll
    , rflSectRoomRoomCount :: Word32
    , rflSectRoomRooms :: V.Vector RFLRoom
    , rflSectRoomMeshCount :: Word32
    , rflSectRoomMeshes :: V.Vector RFLSectionRoomsMeshLink
    , rflSectRoomUnknownCount2 :: Word32
    , rflSectRoomUnknownArr2B :: B.ByteString
    , rflSectRoomVertexCount :: Word32
    , rflSectRoomVertexes :: V.Vector RFV3
    , rflSectRoomFaceCount :: Word32
    , rflSectRoomFaces :: V.Vector RFLFace
    , rflSectRoomLightmapCount :: Word32
    , rflSectRoomLightmaps :: V.Vector RFLSectionRoomsLightmap
    , rflSectRoomUnknown3 :: Maybe B.ByteString
    } deriving (Show)

data RFLSectionLevelInfo = RFLSectionLevelInfo 
    { rflSectLevelInfoUnknown1 :: Word32
    , rflSectLevelInfoName :: B.ByteString
    , rflSectLevelInfoAuthor :: B.ByteString
    , rflSectLevelInfoDate :: B.ByteString
    , rflSectLevelInfoUnknown2 :: Word8
    , rflSectLevelInfoIsMultiplayer :: Word8
    , rflSectLevelInfoUnknownArr :: B.ByteString
    } deriving (Show)

data RFLSectionTGAFiles = RFLSectionTGAFiles
    { rflSectTGACount :: Word32
    , rflSectTGAFiles :: V.Vector B.ByteString
    } deriving (Show)

data RFLSectionVCMFiles = RFLSectionVCMFiles
    { rflSectVCMCount :: Word32
    , rflSectVCMFiles :: V.Vector B.ByteString
    , rflSectVCMUnknownArr :: V.Vector Word32
    } deriving (Show)

data RFLSectionMVFFiles = RFLSectionMVFFiles
    { rflSectMVFCount :: Word32
    , rflSectMVFFiles :: V.Vector B.ByteString
    , rflSectMVFUnknownArr :: V.Vector Word32
    } deriving (Show)

data RFLSectionV3DFiles = RFLSectionV3DFiles
    { rflSectV3DCount :: Word32
    , rflSectV3DFiles :: V.Vector B.ByteString
    , rflSectV3DUnknownArr :: V.Vector Word32
    } deriving (Show)

data RFLSectionVFXFiles = RFLSectionVFXFiles
    { rflSectVFXCount :: Word32
    , rflSectVFXFiles :: V.Vector B.ByteString
    , rflSectVFXUnknownArr :: V.Vector Word32
    } deriving (Show)

data RFLSectLevelProps = RFLSectLevelProps
    { rflSectLevelPropGeoModTexture :: B.ByteString
    , rflSectLevelPropHardness :: Word32
    , rflSectLevelPropColorAmbient :: Word32
    , rflSectLevelPropColorFog :: Word32
    , rflSectLevelPropNearPlane :: Float
    , rflSectLevelPropFarPlane :: Float
    } deriving (Show)

data RFLLight = RFLLight
    { rflLightID :: Word32
    , rflLightClassName :: B.ByteString
    , rflLightUnknownArr48 :: B.ByteString
    , rflLightScriptName :: B.ByteString
    , rflLightUnknownArr57 :: B.ByteString
    } deriving (Show)

data RFLSectX x = RFLSectX 
    { rflSectLightCount :: Word32
    , rflSectLights :: V.Vector x
    } deriving (Show)

data RFLEvent = RFLEvent
    { rflEventID :: Word32
    , rflEventClassName :: B.ByteString
    , rflEventPos :: RFV3
    , rflEventScriptName :: B.ByteString
    , rflEventUnknown1 :: Word8
    , rflEventDelay :: Float
    , rflEventBool1 :: Word8
    , rflEventBool2 :: Word8
    , rflEventInt1 :: Int
    , rflEventInt2 :: Int
    , rflEventStr1 :: Maybe B.ByteString
    , rflEventStr2 :: Maybe B.ByteString
    , rflEventUnknown2 :: B.ByteString
    , rflEventLinksCount :: Word32
    , rflEventLinks :: V.Vector Word32
    , rflEventTransform :: Maybe RFMatrix3
    , rflEventUnknownArr3 :: B.ByteString
    , rflEventColor :: Word32
    } deriving (Show)

data RFLMultiPlayerRespawn = RFLMultiPlayerRespawn
    { rflMPRespawnID :: Word32
    , rflMPPos :: RFV3
    , rflMPTrans :: RFMatrix3
    , rflMPScriptName :: B.ByteString
    , rflMPUnknown1 :: Word8
    , rflMPRedTeam :: Word8
    , rflMPBlueTeam :: Word8
    , rflMPBot :: Word8
    , rflMPUnknown2 :: B.ByteString
    } deriving (Show)

data RFLGasRegion = RFLGasRegion
    { rflGasRegionID :: Word32
    , rflGasRegionClassName :: B.ByteString
    , rflGasRegionPos :: RFV3
    , rflGasRegionTransform :: RFMatrix3
    , rflGasRegionScriptName :: B.ByteString
    , rflGasRegionUnknownArr17 :: B.ByteString
    } deriving (Show)

data RFLClimbingRegion = RFLClimbingRegion
    { rflClimbingRegionID :: Word32
    , rflClimbingRegionClassName :: B.ByteString
    , rflClimbingRegionPos :: RFV3
    , rflClimbingRegionTransform :: RFMatrix3
    , rflClimbingRegionScriptName :: B.ByteString
    } deriving (Show)

data RFLBoltEmitter = RFLBoltEmitter
    { rflBoltEmitterID :: Word32
    , rflBoltEmitterClassName :: B.ByteString
    , rflBoltEmitterPos :: RFV3
    , rflBoltEmitterTransform :: RFMatrix3
    , rflBoltEmitterScriptName :: B.ByteString
    , rflBoltEmitterUnknownArr45 :: B.ByteString
    , rflBoltEmitterTexture :: B.ByteString
    , rflBoltEmitterUnknownArr5 :: B.ByteString
    } deriving (Show)

data RFLSectPlayerStart = RFLSectPlayerStart
    { rflPlayerStartPosition :: RFV3
    , rflPlayerStartTransform :: RFMatrix3
    } deriving (Show)

data RFLSectNavPoints = RFLSectNavPoints 
    { rflSectNavPointsCount :: Word32
    , rflSectNavPoints :: V.Vector RFLNavPoint
    , rflSectNavPointExtra :: V.Vector (V.Vector Word32)
    } deriving (Show)

data RFLNavPoint = RFLNavPoint
    { rflNavPointID :: Word32
    , rflNavPointFloats :: V.Vector Float
    , rflNavPointFlags :: V.Vector Word8
    , rflNavPointUnknownArr1 :: Maybe B.ByteString
    , rflNavPointUnknownArr2 :: B.ByteString
    , rflNavPointLinksCount :: Word32
    , rflNavPointLinks :: V.Vector Word32
    } deriving (Show)

data RFLEntity = RFLEntity
    { rflEntityID :: Word32
    , rflEntityClassName :: B.ByteString
    , rflEntityPos :: RFV3 
    , rflEntityTransform :: RFMatrix3 
    , rflEntityScriptName :: B.ByteString
    , rflEntityUnknown1 :: Word8
    , rflEntityCooperation :: Word32
    , rflEntityFriendliness :: Word32
    , rflEntityTeamID :: Word32
    , rflEntityWaypointList ::  B.ByteString
    , rflEntityWaypointMethod :: B.ByteString
    , rflEntityUnknown2 :: Word8
    , rflEntityBoarded :: Word8
    , rflEntityReadyToFire :: Word8
    , rflEntityOnlyAttackPlayer :: Word8
    , rflEntityWeaponIsHolstered :: Word8
    , rflEntityDeaf :: Word8
    , rflEntitySweepMinAngle :: Float
    , rflEntitySweepMaxAngle :: Float
    , rflEntityIgnoreTerrainWhenFiring :: Word8
    , rflEntityUnknown3 :: Word8
    , rflEntityStartCrouched :: Word8
    , rflEntityLife :: Float
    , rflEntityArmor :: Float
    , rflEntityFOV :: Float
    , rflEntityDefaultPrimaryWeapon :: B.ByteString
    , rflEntityDefaultSecondaryWeapon :: B.ByteString
    , rflEntityItemDrop :: B.ByteString 
    , rflEntityStateAnim :: B.ByteString 
    , rflEntityCorpsePose :: B.ByteString 
    , rflEntitySkin :: B.ByteString 
    , rflEntityDeathAnim :: B.ByteString 
    , rflEntityAIMode :: Word8
    , rflEntityAIAttackStyle :: Word8
    , rflEntityUnknown4 :: Word32
    , rflEntityTurretID :: Word32
    , rflEntityAlertCameraID :: Word32
    , rflEntityAlertEventID :: Word32
    , rflEntityRun :: Word8
    , rflEntityStartHidden :: Word8 
    , rflEntityWearHelmet :: Word8 
    , rflEntityEndGameIfKilled :: Word8 
    , rflEntityCowerFromWeapon :: Word8 
    , rflEntityQuestionUnarmedPlayer :: Word8 
    , rflEntityDontHum :: Word8 
    , rflEntityNoShadow :: Word8 
    , rflEntityAlwaysSimulate :: Word8 
    , rflEntityPerfectAim :: Word8 
    , rflEntityPermanentCorpse :: Word8 
    , rflEntityNeverFly :: Word8 
    , rflEntityNeverLeave :: Word8 
    , rflEntityNoPersonaMessages :: Word8  
    , rflEntityFadeCorpseImmediately :: Word8  
    , rflEntityNeverCollideWithPlayer :: Word8  
    , rflEntityUseCustomAttackRange :: Word8  
    , rflEntityCustomAttackRange :: Maybe Float
    , rflEntityLeftHandHolding :: B.ByteString 
    , rflEntityRightHandHolding :: B.ByteString
    } deriving (Show)

data RFLItem = RFLItem
    { rflItemID :: Word32
    , rflItemClassName :: B.ByteString
    , rflItemPos :: RFV3
    , rflItemTrans :: RFMatrix3
    , rflItemScriptName :: B.ByteString
    , rflItemUnknown1 :: Word8
    , rflItemCount :: Word32
    , rflItemRespawnTime :: Word32
    , rflItemTeamID :: Word32
    } deriving (Show)

data RFLClutter = RFLClutter
    { rflClutterID :: Word32
    , rflClutterClassName :: B.ByteString
    , rflClutterPos :: RFV3
    , rflClutterTrans :: RFMatrix3
    , rflClutterScriptName :: B.ByteString
    , rflClutterUnknownArr5 :: B.ByteString
    , rflClutterSkin :: B.ByteString
    , rflClutterLinksCount :: Word32
    , rflClutterLinks :: V.Vector Word32
    } deriving (Show)

data RFLDecal = RFLDecal
    { rflDecalID :: Word32
    , rflDecalUnknownText1 :: B.ByteString
    , rflDecalFloats :: V.Vector Float
    , rflDecalUnknownText2 :: B.ByteString
    , rflDecalUnknownArr1 :: B.ByteString
    , rflDecalTexture :: B.ByteString
    , rflDecalUnknownArr2 :: B.ByteString
    } deriving (Show)

data RFLAmbientSound = RFLAmbientSound
    { rflAmbientSoundID :: Word32
    , rflAmbientSoundVec1 :: RFV3
    , rflAmbientSoundUnknown1 :: B.ByteString
    , rflAmbientSoundWave :: B.ByteString
    , rflAmbientSoundVec3 :: RFV3
    , rflAmbientSoundUnknown2 :: B.ByteString
    } deriving (Show)

data RFLTrigger = RFLTrigger
    { rflTriggerID :: Word32
    , rflTriggerScriptName :: B.ByteString
    , rflTriggerUnknown1 :: Word8
    , rflTriggerIsBox :: Word8
    , rflTriggerUnknownArr3 :: B.ByteString
    , rflTriggerResetsAfter :: Float
    , rflTriggerResetsCount :: Word16
    , rflTriggerUnknown2 :: Word16
    , rflTriggerIsUseKeyRequired :: Word8
    , rflTriggerKeyName :: B.ByteString
    , rflTriggerWeaponActivates :: Word8
    , rflTriggerUnknown3 :: Word8
    , rflTriggerIsNPC :: Word8
    , rflTriggerIsAuto :: Word8
    , rflTriggerInVehicle :: Word8
    , rflTriggerPos :: RFV3
    , rflTriggerSphereRadius :: Maybe Float
    , rflTriggerBoxTransform :: Maybe RFMatrix3
    , rflTriggerBoxDimensions :: Maybe RFV3
    , rflTriggerOneWay :: Maybe Word8
    , rflTriggerAirlockRoom :: Word32
    , rflTriggerAttachedTo :: Word32
    , rflTriggerUseClutter :: Word32
    , rflTriggerDisabled :: Word8
    , rflTriggerButtonActiveTime :: Float
    , rflTriggerInsideTime :: Float
    , rflTriggerUnknown4 :: Word32
    , rflTriggerLinksCount :: Word32
    , rflTriggerLinks :: V.Vector Word32
    } deriving (Show)

data RFLBrush = RFLBrush
    { rflBrushID :: Word32
    , rflBrushPos :: RFV3
    , rflBrushTrans :: RFMatrix3
    , rflBrushUnknownArr10 :: B.ByteString
    , rflBrushTexturesCount :: Word32
    , rflBrushTextures :: V.Vector B.ByteString
    , rflBrushUnknownArr16 :: B.ByteString
    , rflBrushVertexCount :: Word32
    , rflBrushVertexes :: V.Vector RFV3
    , rflBrushFacesCount :: Word32
    , rflBrushFaces :: V.Vector RFLFace
    , rflBrushUnknownArr1C :: Word32
    , rflBrushUnknownArr1V :: Maybe B.ByteString
    , rflBrushFlags :: RFLBrushFlags
    , rflBrushUnknownFlagsArr :: Maybe B.ByteString
    , rflBrushLife :: Word32
    , rflBrushUnknown2 :: Word32
    , rflBrushUnknown3 :: Maybe Word32
    } deriving (Show)


