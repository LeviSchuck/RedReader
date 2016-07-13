{-# LANGUAGE OverloadedStrings #-}
module Red.Parse.RFL where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Word

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.Binary as AB
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Red.Binary.Base
import Red.Binary.RFL
import Red.Enums.FromBinary
import Red.Enums.RFL

import Red.Parse.Base
import Red.Parse.Prim

data RFLSection
    = RFLSectTGAList RFLSectionTGAFiles
    | RFLSectVCMList RFLSectionVCMFiles
    | RFLSectMVFList RFLSectionMVFFiles
    | RFLSectV3DList RFLSectionV3DFiles
    | RFLSectVFXList RFLSectionVFXFiles
    | RFLSectLP RFLSectLevelProps
    | RFLSectLI RFLSectionLevelInfo
    | RFLSectSG RFLSectionRooms
    | RFLSectBR (RFLSectX RFLBrush)
    | RFLSectCL (RFLSectX RFLClutter)
    | RFLSectLightMapsDummy
    | RFLSectLT (RFLSectX RFLLight)
    | RFLSectIT (RFLSectX RFLItem)
    | RFLSectTR (RFLSectX RFLTrigger)
    | RFLSectET (RFLSectX RFLEntity)
    | RFLSectEV (RFLSectX RFLEvent)
    | RFLSectMP (RFLSectX RFLMultiPlayerRespawn)
    | RFLSectRE (RFLSectX RFLRoomEffect)
    | RFLSectDC (RFLSectX RFLDecal)
    | RFLSectEAX (RFLSectX RFLEAXEffect)
    | RFLSectAB (RFLSectX RFLAmbientSound)
    | RFLSectNVP RFLSectNavPoints
    | RFLSectPS RFLSectPlayerStart
    | RFLSectEnd
    | RFLSectUnknown B.ByteString
    deriving (Show)

data RFL = RFL
    { rflHeader :: RFLHeader
    , rflSections :: [(RFLSectionHeader, RFLSection)]
    } deriving (Show)

data RFContext = RFContext
    { rfcVersion :: Word32
    } deriving (Show)


parseRFLHeader :: AP.Parser RFLHeader
parseRFLHeader = do
    _ <- AB.word32le rflSignature
    v <- AB.anyWord32le
    ut <- AB.anyWord32le
    ops <- AB.anyWord32le
    oli <- AB.anyWord32le
    sc <- AB.anyWord32le
    u <- AB.anyWord32le
    ln <- parseLengthedText
    mn <- parseLengthedText
    return $ RFLHeader v u ops oli sc u ln mn

parseRFLSectionHeader :: AP.Parser RFLSectionHeader
parseRFLSectionHeader = do
    scb <- AB.anyWord32le
    let sc = fromBinary32 scb
    l <- AB.anyWord32le
    return $ RFLSectionHeader sc l

parseRFLLevelProperties :: AP.Parser RFLSectLevelProps
parseRFLLevelProperties = do
    tex <- parseLengthedText
    h <- AB.anyWord32le
    ca <- AB.anyWord32le
    cf <- AB.anyWord32le
    npw <- AB.anyWord32le
    let np = wordToFloat npw
    fpw <- AB.anyWord32le
    let fp = wordToFloat fpw
    return $ RFLSectLevelProps tex h ca cf np fp

parseRFLSectionLevelInfo :: AP.Parser RFLSectionLevelInfo
parseRFLSectionLevelInfo = do
    u1 <- AB.anyWord32le
    n <- parseLengthedText
    a <- parseLengthedText
    d <- parseLengthedText
    u2 <- AP.anyWord8
    m <- AP.anyWord8
    u3 <- AP.take 220
    return $ RFLSectionLevelInfo u1 n a d u2 m u3

parseRFLLight :: AP.Parser RFLLight
parseRFLLight = do
    i <- AB.anyWord32le
    cn <- parseLengthedText
    ua1 <- AP.take 48
    sn <- parseLengthedText
    ua2 <- AP.take 57
    return $ RFLLight i cn ua1 sn ua2

parseRFLClutter :: AP.Parser RFLClutter
parseRFLClutter = do
    i <- AB.anyWord32le
    cn <- parseLengthedText
    pos <- parseV3
    tr <- parseMatrix3
    sn <- parseLengthedText
    ua1 <- AP.take 5
    sk <- parseLengthedText
    lC <- AB.anyWord32le
    lV <- parseVector (fromIntegral lC) AB.anyWord32le
    return $ RFLClutter i cn pos tr sn ua1 sk lC lV

parseRFLTrigger :: AP.Parser RFLTrigger
parseRFLTrigger = do
    i <- AB.anyWord32le
    -- traceM ("Trigger ID: " ++ show i)
    sn <- parseLengthedText
    -- traceM ("Trigger Name: " ++ show sn)
    u1 <- AP.anyWord8
    ib <- AP.anyWord8
    -- traceM ("Is Box: " ++ show ib)
    ua2 <- AP.take 3
    ra <- parseFloat
    rc <- AB.anyWord16le
    u3 <- AB.anyWord16le
    ukr <- AP.anyWord8
    kn <- parseLengthedText
    -- traceM ("Trigger key name: " ++ show kn)
    wa <- AP.anyWord8
    u4 <- AP.anyWord8
    inpc <- AP.anyWord8
    iau <- AP.anyWord8
    iiv <- AP.anyWord8
    pos <- parseV3
    (sr, bt, bd, ow) <- if ib == 0
        then do
            s <- parseFloat
            return (Just s, Nothing, Nothing, Nothing)
        else do
            bt <- parseMatrix3
            bd <- parseV3
            ow <- AP.anyWord8
            return (Nothing, Just bt, Just bd, Just ow)

    ar <- AB.anyWord32le
    at <- AB.anyWord32le
    uc <- AB.anyWord32le
    id <- AP.anyWord8
    bat <- parseFloat
    ist <- parseFloat
    u5 <- AB.anyWord32le
    lC <- AB.anyWord32le
    -- traceM ("Trigger link count: " ++ show lC)
    lV <- parseVector (fromIntegral lC) AB.anyWord32le
    return $ RFLTrigger i sn u1 ib ua2 ra rc u3 ukr kn wa u4 inpc iau iiv pos sr bt bd ow ar at uc id bat ist u5 lC lV



parseRFLSectionRooms :: RFContext -> AP.Parser RFLSectionRooms
parseRFLSectionRooms c = do
    a1 <- if rfcVersion c == 0xB4
        then AP.take 6
        else AP.take 10
    texC <- AB.anyWord32le
    -- traceM ("Rooms have " ++ show texC ++ " textures")
    texV <- parseVector (fromIntegral texC) parseLengthedText
    scrC <- AB.anyWord32le
    scrV <- parseVector (fromIntegral scrC) parseRFLFaceScroll
    roomC <- AB.anyWord32le
    -- traceM ("There are " ++ show roomC ++ " room effects")
    (rbs, roomL) <- AP.match $ replicateM (fromIntegral roomC) (do
        (bs, r) <- AP.match (parseRFLRoom c)
        -- traceM ("Got room effect: " ++ show r)
        -- traceM ("Room Bin: " ++ hexPrint bs)
        return r)
    -- traceM ("Rooms look like: " ++ hexPrint rbs)
    let roomV = V.fromList roomL
    meshC <- AB.anyWord32le
    -- traceM ("There are " ++ show meshC ++ " meshes")
    when (meshC /= roomC) (do
        traceM "There are supposed to be an equivalent number of rooms to meshes!"
        )
    meshV <- parseVector (fromIntegral meshC) parseRFLSectionRoomsMeshLink
    unC <- AB.anyWord32le
    -- traceM ("There are " ++ show unC ++ " unknown 32-byte segments")
    unB <- AP.take ((fromIntegral unC) * 32)
    vecC <- AB.anyWord32le
    -- traceM ("There are " ++ show vecC ++ " vertexes")
    vecV <- parseVector (fromIntegral vecC) parseV3
    faceC <- AB.anyWord32le
    -- traceM ("There are " ++ show faceC ++ " faces")
    faceV <- parseVector (fromIntegral faceC) parseRFLFace
    lmC <- AB.anyWord32le
    -- traceM ("There are " ++ show lmC ++ " light maps")
    lmV <- parseVector (fromIntegral lmC) parseRFLSectionRoomsLightmap
    un3 <- if rfcVersion c == 0xB4
        then do
            bs <- AP.take 4
            return $ Just bs
        else return Nothing
    return $ RFLSectionRooms a1 texC texV scrC scrV roomC roomV meshC meshV unC unB vecC vecV faceC faceV lmC lmV un3

parseRFLVertex :: Bool -> AP.Parser RFLVertex
parseRFLVertex useLM = do
    i <- AB.anyWord32le
    tuv <- parseUV
    luv <- if useLM
        then do
            uv <- parseUV
            return $ Just uv
        else return Nothing
    return $ RFLVertex i tuv luv

parseRFLRoom :: RFContext -> AP.Parser RFLRoom
parseRFLRoom c = do
    i <- AB.anyWord32le
    -- traceM ("Room ID: " ++ show i)
    aabb <- parseAABB
    isr <- AP.anyWord8
    ic <- AP.anyWord8
    io <- AP.anyWord8
    ia <- AP.anyWord8
    il <- AP.anyWord8
    ial <- AP.anyWord8
    is <- AP.anyWord8
    un1 <- AP.anyWord8
    l <- parseFloat
    -- traceM ("Life: " ++ show l)
    eax <- parseLengthedText
    -- traceM ("Effects: " ++ show eax)
    when (B.length eax > 120) $ do
        traceM ("Effects is too long!!")
        forever (return ())
    p <- parseRFLRoomProperties il ial
    return $ RFLRoom i aabb isr ic io ia il ial is un1 l eax p

parseRFLRoomProperties :: Word8 -> Word8 -> AP.Parser [RFLRoomProperties]
parseRFLRoomProperties il ial = do
    let amb = do
            c <- AB.anyWord32le
            -- traceM ("Got ambient light " ++ niceHex32 c)
            return $ RFLRoomPropertiesAmbient c
        liq = do
            d <- parseFloat
            c <- AB.anyWord32le
            st <- parseLengthedText
            v <- parseFloat
            lt <- AB.anyWord32le
            la <- AB.anyWord32le
            u1 <- AP.take 13
            w <- parseFloat
            uv <- parseUV
            return $ RFLRoomPropertiesLiquid d c st v lt la u1 w uv
    case (ial > 0, il > 0) of
        (True,True) -> do
            l <- liq
            a <- amb
            return $ a : l : []
        (True,_) -> do
            a <- amb
            return $ a : []
        (_,True) -> do
            l <- liq
            return $ l : []
        _ -> return [RFLRoomPropertiesNone]

parseRFLFaceScroll :: AP.Parser RFLFaceScroll
parseRFLFaceScroll = do
    i <- AB.anyWord32le
    uv <- parseUV
    return $ RFLFaceScroll i uv

parseRFLNavPoint :: RFContext -> AP.Parser RFLNavPoint
parseRFLNavPoint c = do
    i <- AB.anyWord32le
    -- traceM ("ID: " ++ show i)
    fs <- parseVector 6 parseFloat
    b1 <- AP.anyWord8
    b2 <- AP.anyWord8
    b3 <- AP.anyWord8
    b4 <- AP.anyWord8
    b5 <- AP.anyWord8
    let flagsV = V.fromList [b1,b2,b3,b4,b5]
    ua1 <- if b2 > 0
        then do
            bs <- AP.take 36
            return $ Just bs
        else return Nothing
    ua2 <- AP.take 4
    -- traceM ("Bytes: " ++ hexPrint ua1)
    linksCount <- AB.anyWord32le
    -- traceM ("Links: " ++ niceHex32 linksCount)
    ls <- parseVector (fromIntegral linksCount) AB.anyWord32le
    return $ RFLNavPoint i fs flagsV ua1 ua2 linksCount ls


parseRFLFace :: AP.Parser RFLFace
parseRFLFace = do
    n <- parseV4
    -- traceM ("Face normal: " ++ show n)
    t <- AB.anyWord32le
    lmti <- AB.anyWord32le
    u2 <- AB.anyWord32le
    ua8 <- AP.take 8
    u3 <- AB.anyWord32le
    ffw <- AP.anyWord8
    let ff = fromBinary8 ffw
    lr <- AP.anyWord8
    ua6 <- AP.take 6
    ri <- AB.anyWord32le
    vC <- AB.anyWord32le
    when (vC > 100) $ do
        traceM ("Face reports " ++ show vC ++ " vertexes. This is probably bad.")
    vV <- parseVector (fromIntegral vC) (parseRFLVertex (lmti /= 0xFFFFFFFF))
    -- traceM ("Vertexes look like: " ++ show vV)
    return $ RFLFace n t lmti u2 ua8 u3 ff lr ua6 ri vC vV

parseRFLItem :: AP.Parser RFLItem
parseRFLItem = do
    i <- AB.anyWord32le
    cn <- parseLengthedText
    pos <- parseV3
    tr <- parseMatrix3
    sn <- parseLengthedText
    u1 <- AP.anyWord8
    c <- AB.anyWord32le
    rt <- AB.anyWord32le
    ti <- AB.anyWord32le
    return $ RFLItem i cn pos tr sn u1 c rt ti

parseRFLEvent :: AP.Parser RFLEvent
parseRFLEvent = do
    {-
    bs <- AP.take 4000
    traceM ("Bytes " ++ hexPrint bs)
    forever (return ())
    -}
    i <- AB.anyWord32le
    -- traceM ("Event ID: " ++ show i)
    cn <- parseLengthedText
    -- traceM ("Event Name: " ++ show cn)
    when (B.length cn > 80) (do
        traceM "What is the going on? > 80"
        forever (return ()))
    pos <- parseV3
    -- traceM ("Event vec: " ++ show pos)
    sn <- parseLengthedText
    -- traceM ("Event " ++ show sn)
    when (B.length sn > 80) (do
        traceM "What is the going on? > 80"
        forever (return ()))
    u1 <- AP.anyWord8
    -- traceM ("Event U1: " ++ show u1)
    del <- parseFloat
    -- traceM ("Event Delay: " ++ show del)
    b1 <- AP.anyWord8
    -- traceM ("Bool 1: " ++  show b1)
    b2 <- AP.anyWord8
    -- traceM ("Bool 2: " ++  show b2)
    i1 <- parseInt
    -- traceM ("Int 1: " ++  show i1)
    i2 <- parseInt
    -- traceM ("Int 2: " ++  show i2)
    ua2 <- AP.take 8
    (ua3,s1,s2) <- do
        let mode1 = if (any (cn ==)
                [
                ])
                then Just ()
                else Nothing
        let mode2 = if (any (cn ==)
                [ "When_Dead"
                ])
                then Just ()
                else Nothing
        case (mode1,mode2) of
            (Just _,_) -> do
                -- traceM ("Taking 8 on CN: " ++ show cn)
                bs <- AP.take 8
                return (bs, Nothing, Nothing)
            (_, Just _) -> do
                bs <- AP.take 4
                return (bs, Nothing, Nothing)
            _ -> do
                s1 <- parseLengthedText
                s2 <- parseLengthedText
                bs <- AP.take 0
                return (bs, Just s1, Just s2)
    -- traceM ("Unknown arr: " ++ hexPrint ua3)
    -- traceM ("Str1: " ++ show s1)
    -- traceM ("Str2: " ++ show s2)
    lC <- AB.anyWord32le
    -- traceM ("Event has " ++ show lC ++ " links")
    lV <- parseVector (fromIntegral lC) AB.anyWord32le
    et <- if (any (cn ==)
        [ "Alarm"
        , "Teleport"
        , "Teleport_Player"
        , "Play_Vclip"
        ])
        then do
            m3 <- parseMatrix3
            return $ Just m3
        else return Nothing
    -- traceM ("Transform: " ++ show et )
    col <- AB.anyWord32le
    -- traceM ("Color: " ++ niceHex32 col)
    return $ RFLEvent i cn pos sn u1 del b1 b2 i1 i2 s1 s2 ua2 lC lV et ua3 col

parseRFLSectionRoomsMeshLink :: AP.Parser RFLSectionRoomsMeshLink
parseRFLSectionRoomsMeshLink = do
    i <- AB.anyWord32le
    lC <- AB.anyWord32le
    lV <- parseVector (fromIntegral lC) AB.anyWord32le
    return $ RFLSectionRoomsMeshLink i lC lV

parseRFLSectionRoomsLightmap :: AP.Parser RFLSectionRoomsLightmap
parseRFLSectionRoomsLightmap = do
    li <- AB.anyWord32le
    ua <- AP.take 88
    fi <- AB.anyWord32le
    return $ RFLSectionRoomsLightmap li ua fi


parseRFLAmbientSound :: RFContext -> AP.Parser RFLAmbientSound
parseRFLAmbientSound c = do
    i <- AB.anyWord32le
    v1 <- parseV3
    ua1 <- AP.take 1
    wav <- parseLengthedText
    v2 <- parseV3
    ua2 <- AP.take 4
    return $ RFLAmbientSound i v1 ua1 wav v2 ua2

parseRFLEAXEffect :: RFContext -> AP.Parser RFLEAXEffect
parseRFLEAXEffect c = do
    ty <- parseLengthedText
    -- traceM ("EAX: " ++ show ty)
    ua1 <- AP.take 4
    un1 <- parseLengthedText
    pars <- replicateM 12 parseFloat
    un2 <- parseLengthedText
    ua2 <- AP.take 1
    return $ RFLEAXEffect ty ua1 un1 pars un2 ua2

parseRFLSectionMVFFiles :: AP.Parser RFLSectionMVFFiles
parseRFLSectionMVFFiles = do
    mvfC <- AB.anyWord32le
    mvfV <- parseVector (fromIntegral mvfC) parseLengthedText
    uaV <- parseVector (fromIntegral mvfC) AB.anyWord32le
    return $ RFLSectionMVFFiles mvfC mvfV uaV

parseRFLSectionVCMFiles :: AP.Parser RFLSectionVCMFiles
parseRFLSectionVCMFiles = do
    vcmC <- AB.anyWord32le
    vcmV <- parseVector (fromIntegral vcmC) parseLengthedText
    uaV <- parseVector (fromIntegral vcmC) AB.anyWord32le
    return $ RFLSectionVCMFiles vcmC vcmV uaV

parseRFLSectionVFXFiles :: AP.Parser RFLSectionVFXFiles
parseRFLSectionVFXFiles = do
    vfxC <- AB.anyWord32le
    vfxV <- parseVector (fromIntegral vfxC) parseLengthedText
    uaV <- parseVector (fromIntegral vfxC) AB.anyWord32le
    return $ RFLSectionVFXFiles vfxC vfxV uaV

parseRFLSectionV3DFiles :: AP.Parser RFLSectionV3DFiles
parseRFLSectionV3DFiles = do
    v3dC <- AB.anyWord32le
    v3dV <- parseVector (fromIntegral v3dC) parseLengthedText
    uaV <- parseVector (fromIntegral v3dC) AB.anyWord32le
    return $ RFLSectionV3DFiles v3dC v3dV uaV

parseRFLSectionTGAFiles :: AP.Parser RFLSectionTGAFiles
parseRFLSectionTGAFiles = do
    tgaC <- AB.anyWord32le
    tgaV <- parseVector (fromIntegral tgaC) parseLengthedText
    return $ RFLSectionTGAFiles tgaC tgaV

parseRFLSectX :: (Show x) => AP.Parser x -> AP.Parser (RFLSectX x)
parseRFLSectX fun = do
    bC <- AB.anyWord32le
    bV <- parseVector (fromIntegral bC) fun
    return $ RFLSectX bC bV

parseRFLSectNavPoints :: RFContext -> AP.Parser RFLSectNavPoints
parseRFLSectNavPoints c = do
    count <- AB.anyWord32le
    ps <- parseVector (fromIntegral count) $ parseRFLNavPoint c
    ext <- parseVector (fromIntegral count) $ do
        c <- AP.anyWord8
        parseVector (fromIntegral c) AB.anyWord32le
    return $ RFLSectNavPoints count ps ext

parseRFLDecal :: RFContext -> AP.Parser RFLDecal
parseRFLDecal c = do
    i <- AB.anyWord32le
    t1 <- parseLengthedText
    fs <- parseVector 12 parseFloat
    t2 <- parseLengthedText
    ua1 <- AP.take 13
    tx <- parseLengthedText
    ua2 <- AP.take 13
    return $ RFLDecal i t1 fs t2 ua1 tx ua2

parseRFLMultiPlayerRespawn :: RFContext -> AP.Parser RFLMultiPlayerRespawn
parseRFLMultiPlayerRespawn c = do
    i <- AB.anyWord32le
    -- traceM ("Respawn ID: " ++ show i)
    pos <- parseV3
    tr <- parseMatrix3
    sn <- parseLengthedText
    -- traceM ("Script Name: " ++ show sn)
    u1 <- AP.anyWord8
    rt <- AP.anyWord8
    bt <- AP.anyWord8
    b <- AP.anyWord8
    u2 <- AP.take 4
    return $ RFLMultiPlayerRespawn i pos tr sn u1 rt bt b u2

parseRFLEntity :: RFContext -> AP.Parser RFLEntity
parseRFLEntity c = do
    i <- AB.anyWord32le
    -- traceM ("Entity ID: " ++ show i)
    cn <- parseLengthedText
    pos <- parseV3
    tr <- parseMatrix3
    sn <- parseLengthedText
    u1 <- AP.anyWord8
    co <- AB.anyWord32le
    fr <- AB.anyWord32le
    ti <- AB.anyWord32le
    wl <- parseLengthedText
    wm <- parseLengthedText
    u2 <- AP.anyWord8
    bo <- AP.anyWord8
    rtf <- AP.anyWord8
    oap <- AP.anyWord8
    wih <- AP.anyWord8
    id <- AP.anyWord8
    sma1 <- parseFloat
    sma2 <- parseFloat
    itwf <- AP.anyWord8
    u3 <- AP.anyWord8
    sc <- AP.anyWord8
    l <- parseFloat
    ar <- parseFloat
    fov <- parseFloat
    dpw <- parseLengthedText
    dsw <- parseLengthedText
    itd <- parseLengthedText
    sa <- parseLengthedText
    cp <- parseLengthedText
    sk <- parseLengthedText
    da <- parseLengthedText
    aim <- AP.anyWord8
    aias <- AP.anyWord8
    u4 <- AB.anyWord32le
    tid <- AB.anyWord32le
    acid <- AB.anyWord32le
    aeid <- AB.anyWord32le
    run <- AP.anyWord8
    sh <- AP.anyWord8
    wh <- AP.anyWord8
    egik <- AP.anyWord8
    cfw <- AP.anyWord8
    qup <- AP.anyWord8
    dh <- AP.anyWord8
    ns <- AP.anyWord8
    as <- AP.anyWord8
    pa <- AP.anyWord8
    pc <- AP.anyWord8
    nf <- AP.anyWord8
    nl <- AP.anyWord8
    npm <- AP.anyWord8
    fci <- AP.anyWord8
    ncwp <-AP.anyWord8
    ucar <- AP.anyWord8
    car <- if ucar /= 0
        then do
            f <- parseFloat
            return $ Just f
        else return Nothing
    lhh <- parseLengthedText
    rhh <- parseLengthedText
    return $ RFLEntity i cn pos tr sn u1 co fr ti wl wm u2 bo rtf oap wih id sma1 sma2 itwf u3 sc l ar fov dpw dsw itd sa cp sk da aim aias u4 tid acid aeid run sh wh egik cfw qup dh ns as pa pc nf nl npm fci ncwp ucar car lhh rhh

parseRFLRoomEffect :: RFContext -> AP.Parser RFLRoomEffect
parseRFLRoomEffect c = do
    mode <- AB.anyWord32le
    case mode of
        1 -> do
            ua1 <- AP.take 7
            n1 <- parseLengthedText
            ua2 <- AP.take 48
            n2 <- parseLengthedText
            ua3 <- AP.take 1
            return $ RFLRoomEffect1 ua1 n1 ua2 n2 ua3
        2 -> do
            ua1 <- AP.take 8
            n1 <- parseLengthedText
            ua2 <- AP.take 40
            n2 <- parseLengthedText
            ua3 <- AP.take 48
            n3 <- parseLengthedText
            ua4 <- AP.take 1
            return $ RFLRoomEffect2 ua1 n1 ua2 n2 ua3 n3 ua4
        3 -> do
            ua1 <- AP.take 11
            n1 <- parseLengthedText
            ua2 <- AP.take 48
            n2 <- parseLengthedText
            ua3 <- AP.take 1
            return $ RFLRoomEffect3 ua1 n1 ua2 n2 ua3
        4 -> do
            ua1 <- AP.take 7
            n1 <- parseLengthedText
            ua2 <- AP.take 48
            n2 <- parseLengthedText
            ua3 <- AP.take 1
            return $ RFLRoomEffect4 ua1 n1 ua2 n2 ua3
        _ -> do
            traceM ("Unknown room effect mode: " ++ show mode)
            return $ error "Unknown room effect mode"

parseRFLSectPlayerStart :: RFContext -> AP.Parser RFLSectPlayerStart
parseRFLSectPlayerStart c = do
    pos <- parseV3
    tr <- parseMatrix3
    return $ RFLSectPlayerStart pos tr

parseRFLBrush :: RFContext -> AP.Parser RFLBrush
parseRFLBrush c = do
    -- traceM "------------"
    i <- AB.anyWord32le
    -- traceM ("Brush has ID: " ++ show i)
    pos <- parseV3
    tr <- parseMatrix3
    ua1 <- if (rfcVersion c) == 0xB4
        then AP.take 6
        else AP.take 10
    -- traceM ("UA1: " ++ hexPrint ua1)
    texC <- AB.anyWord32le
    -- traceM ("Brush has " ++ show texC ++ " textures")
    texV <- parseVector (fromIntegral texC) parseLengthedText
    -- traceM ("Brush textures: " ++ show texL)
    ua2C <- AB.anyWord32le
    -- traceM ("(Unknown vec3 count): " ++ show ua2C)
    ua2 <- AP.take ((fromIntegral ua2C) * 12 + 12)
    -- traceM ("Brush UA2: " ++ hexPrint ua2)
    vecC <- AB.anyWord32le
    -- traceM ("Brush has " ++ show vecC ++ " vertexes")
    vecV <- parseVector (fromIntegral vecC) parseV3
    faceC <- AB.anyWord32le
    -- traceM ("Brush has " ++ show faceC ++ " faces")
    faceV <- parseVector (fromIntegral faceC) parseRFLFace
    un1 <- AB.anyWord32le
    -- traceM ("Un1: " ++ niceHex32 un1)
    ua3 <- if un1 /= 0
        then do
            bs <- AP.take ((fromIntegral un1) * 96)
            return $ Just bs
        else return Nothing
    bfw <- AB.anyWord32le
    let bf = fromBinary32 bfw
    -- traceM ("Flags: " ++ show bf)
    {-when (i == 8357) $ do
        traceM "Dumping"
        bs <- AP.take 500
        traceM $ hexPrint bs
        forever $ return ()
    -}
    ufa <- if (rfcVersion c) == 0xB4
        then case bf of
            RFLBFEmitStream -> do
                bs <- AP.take 192
                return $ Just bs
            RFLBFAir -> do
                bs <- AP.take 24
                return $ Just bs
            RFLBFPortalAir -> do
                bs <- AP.take 36
                return $ Just bs
            RFLBFPortal -> do
                bs <- AP.take 12
                return $ Just bs
            RFLBFDetail -> do
                bs <- AP.take 48
                return $ Just bs
            _ -> return Nothing
        else return Nothing
    bl <- AB.anyWord32le
    -- traceM ("Life: " ++ show bl)
    un2 <- AB.anyWord32le
    -- traceM ("Un2: 0x" ++ niceHex32 un2)
    un3 <- if (rfcVersion c) == 0xB4
        then do
            w <- AB.anyWord32le
            return $ Just w
        else return Nothing
    -- traceM ("Un3: " ++ show un3)
    return $ RFLBrush i pos tr ua1 texC texV ua2 vecC vecV faceC faceV un1 ua3 bf ufa bl un2 un3

parseRFLSection :: RFContext -> RFLSectionHeader -> AP.Parser RFLSection
parseRFLSection c m = case rflSectionType m of
    RFLLevelProperties -> do
        lp <- parseRFLLevelProperties
        return $ RFLSectLP lp
    RFLStaticGeo -> do
        sc <- parseRFLSectionRooms c
        return $ RFLSectSG sc
    RFLLights -> do
        ls <- parseRFLSectX parseRFLLight
        return $ RFLSectLT ls
    RFLNavPoints -> do
        navs <- parseRFLSectNavPoints c
        return $ RFLSectNVP navs
    RFLClutters -> do
        cs <- parseRFLSectX parseRFLClutter
        return $ RFLSectCL cs
    RFLBrushes -> do
        br <- parseRFLSectX (parseRFLBrush c)
        return $ RFLSectBR br
    RFLItems -> do
        is <- parseRFLSectX parseRFLItem
        return $ RFLSectIT is
    RFLTriggers -> do
        ts <- parseRFLSectX parseRFLTrigger
        return $ RFLSectTR ts
    RFLEntities -> do
        et <- parseRFLSectX (parseRFLEntity c)
        return $ RFLSectET et
    RFLEvents -> do
        evs <- parseRFLSectX parseRFLEvent
        return $ RFLSectEV evs
    RFLDecals -> do
        dcs <- parseRFLSectX (parseRFLDecal c)
        return $ RFLSectDC dcs
    RFLMultiPlayerRespawns -> do
        mps <- parseRFLSectX (parseRFLMultiPlayerRespawn c)
        return $ RFLSectMP mps
    RFLRoomEffects -> do
        res <- parseRFLSectX (parseRFLRoomEffect c)
        return $ RFLSectRE res
    RFLEAXEffects -> do
        eax <- parseRFLSectX (parseRFLEAXEffect c)
        return $ RFLSectEAX eax
    RFLAmbientSounds -> do
        sds <- parseRFLSectX (parseRFLAmbientSound c)
        return $ RFLSectAB sds
    RFLLevelInfo -> do
        li <- parseRFLSectionLevelInfo
        return $ RFLSectLI li
    RFLMVFList -> do
        li <- parseRFLSectionMVFFiles
        return $ RFLSectMVFList li
    RFLTGAList -> do
        li <- parseRFLSectionTGAFiles
        return $ RFLSectTGAList li
    RFLV3DList -> do
        li <- parseRFLSectionV3DFiles
        return $ RFLSectV3DList li
    RFLVFXList -> do
        li <- parseRFLSectionVFXFiles
        return $ RFLSectVFXList li
    RFLVCMList -> do
        li <- parseRFLSectionVCMFiles
        return $ RFLSectVCMList li
    RFLPlayerStart -> do
        st <- parseRFLSectPlayerStart c
        return $ RFLSectPS st
    RFLEnd -> return RFLSectEnd
    RFLLightMaps -> do
        _ <- AP.take (fromIntegral (rflSectionLength m))
        return $ RFLSectLightMapsDummy
    _ -> do
        bytes <- AP.take (fromIntegral (rflSectionLength m))
        return $ RFLSectUnknown bytes

parseRFL :: AP.Parser RFL
parseRFL = do
    traceM "About to parse header"
    h <- parseRFLHeader
    let c = RFContext (rflVersion h)
    traceM ("Got header " ++ show h)
    let rs = do
            traceM "Gonna try to parse a section"
            hd <- parseRFLSectionHeader
            traceM ("Header " ++ show hd)
            (bs, s) <- AP.match (parseRFLSection c hd)
            -- traceM ("Body " ++ show s)
            if ((B.length bs) /= fromIntegral (rflSectionLength hd))
                then do
                    traceM ("Length does not match precisely, expected "
                            ++ show (fromIntegral (rflSectionLength hd))
                            ++ " but read "
                            ++ show (B.length bs)
                            ++ " difference is "
                            ++ show (fromIntegral (rflSectionLength hd) - B.length bs)
                            )
                    let rm = (fromIntegral (rflSectionLength hd)) - (B.length bs)
                    if rm < 0
                        then traceM "Read too many bytes D:"
                        else do
                            ex <- AP.take rm
                            traceM ("Reading remainder bytes.. " ++ hexPrint ex)
                else do
                    traceM "Read precisely expected bytes"
                    return ()

            return (hd, s)
        rsects = do
            r <- rs
            let (hd,_) = r
            e <- AP.atEnd
            case (rflSectionType hd,e) of
                (RFLEnd,_) -> return []
                (_,True) -> return []
                _ -> do
                    rest <- rsects
                    return (r:rest)
    scs <- rsects
    traceM "Returning"
    -- AP.endOfInput
    return $ RFL h scs
