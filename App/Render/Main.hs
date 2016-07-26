{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings, RecordWildCards #-}

import System.Environment
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Data.Text (unpack,Text)
import Data.List (groupBy,nub,sortOn)
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import Data.String(fromString)
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.ByteString as SB

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL

import Codec.Picture as Juicy
import Data.Aeson

import Red.Binary.Base
import Red.Binary.RFL
import Red.Binary.VPP
import Red.Parse.Base
import Red.Parse.RFL
import Red.Parse.VPP

import Red.Support.FileSystem

import qualified Graphics.Mesh as G
import Graphics.Math

baseVPPs =
    [ "maps1.vpp"
    , "maps2.vpp"
    , "maps3.vpp"
    , "maps_en.vpp"
    , "levels1.vpp"
    , "levels2.vpp"
    , "levels3.vpp"
    , "levelsm.vpp"
    , "ui.vpp"
    , "tables.vpp"
    , "motions.vpp"
    , "meshes.vpp"
    ]

vc :: V.Vector (V.Vector x) -> V.Vector x
vc = V.concat . V.toList

loadTextures vpps _          m Nothing     = return m
loadTextures vpps defaultTex m (Just tris) = foldM loadTexture m tris where
    loadTexture m (_, tex, _) = case M.lookup (tex :: SB.ByteString) (m :: M.Map SB.ByteString TextureData) of
        Just _ -> return m
        Nothing -> do
            -- traceM ("Loading texture: " ++ show tex)
            vppBytes <- loadFile vpps tex
            case vppBytes of
                Nothing -> do
                    traceM ("Could not find " ++ show tex ++ " in VPP")
                    return $ M.insert tex defaultTex m
                Just imgbytes -> case Juicy.decodeImage imgbytes of
                    Left _ -> do
                        traceM ("Could not decode image " ++ show tex)
                        return $ M.insert tex defaultTex m
                    Right teximg -> do
                        gpuimg <- LambdaCubeGL.uploadTexture2DToGPU teximg
                        return $ M.insert tex gpuimg m



loadMeshes storage texes (r, texname, (pos,vns,uvs)) = do
    let m = Mesh
            { mAttributes = M.fromList
                [ ("position", A_V4F (vc pos))
                , ("normal", A_V4F (vc vns))
                , ("uvw", A_V3F (vc uvs))
                ]
            , mPrimitive = P_Triangles
            }
        identm :: V4 (V4 Float)
        identm = V4 (V4 1.0 0 0 0) (V4 0 1.0 0 0) (V4 0 0 1.0 0) (V4 0 0 0 1.0)
    gm <- LambdaCubeGL.uploadMeshToGPU m
    obj <- LambdaCubeGL.addMeshToObjectArray storage "objects" ["diffuse","meshTransform"] gm
    case M.lookup (texname :: SB.ByteString) (texes :: M.Map SB.ByteString TextureData) of
        Nothing -> return ()
        Just x -> LambdaCubeGL.updateObjectUniforms obj $ do
            "diffuse" @= return x
            "meshTransform" @= return identm
    return (r, obj)


roomsToMeshes rooms = trisub where
    nrf n = V4 (rfV4x n) (rfV4y n) (rfV4z n) (rfV4w n)
    uvc uv = V3 (rfu uv) (rfv uv) 1.0
    vpos = V.map (\p -> V4 (rfV3x p) (rfV3y p) (rfV3z p) 1.0) (rflSectRoomVertexes rooms)
    f :: RFLFace
      -> V.Vector
          ( (V4 Float, V4 Float, V3 Float)
          , (V4 Float, V4 Float, V3 Float)
          , (V4 Float, V4 Float, V3 Float)
          , Int
          , Int
          )
    f face = tris where
        nm = nrf $ rflFaceNormal face
        fvs = rflFaceVertexes face
        tex :: Int
        tex = fromIntegral (rflFaceTextureID face)
        ri :: Int
        ri = fromIntegral (rflFaceRoomIndex face)
        uvs = V.map (uvc.rflTexUV) fvs
        pss = V.map (\fv -> vpos V.! (fromIntegral (rflVIndex fv))) fvs
        num = (V.length fvs) - 2
        verts = V.map (\(a,b) -> (a,nm,b)) $ V.zip pss uvs
        v0 = V.head verts
        v1s = V.drop 1 verts
        v2s = V.drop 2 verts
        v12s = V.zip v1s v2s

        tris = V.map (\(a,b) -> (v0, a, b, tex, ri)) v12s

    faces :: V.Vector
            ( V.Vector
                ( (V4 Float, V4 Float, V3 Float)
                , (V4 Float, V4 Float, V3 Float)
                , (V4 Float, V4 Float, V3 Float)
                , Int
                , Int
                )
            )
    faces = V.map f (rflSectRoomFaces rooms)
    totris :: V.Vector (V.Vector (V4 Float), V.Vector (V4 Float), V.Vector (V3 Float), Int, Int)
    totris = f3 (vc faces) where
        f3 vs = V.map f4 vs
        f4 ((p1,n1,u1),(p2,n2,u2),(p3,n3,u3),t, r) =
            ( V.fromList [p1,p2,p3]
            , V.fromList [n1,n2,n3]
            , V.fromList [u1,u2,u3]
            , t
            , r
            )
    gt (_,_,_,t,r) = (r, t)
    textris = groupBy (\ t1 t2 -> gt t1 == gt t2) (sortOn gt (V.toList totris))
    trisub = map trifun textris where
        -- trifun :: [(_,_,_, Int)] -> [(SB.ByteString, [()])
        trifun [] = (0, SB.empty, (V.empty, V.empty, V.empty))
        trifun xs@(x:_) = (r, tex, f1 (map tr xs)) where
            (r, tex) = (\(_,_,_,t,r) -> (r, rflSectRoomTextures rooms V.! t)) x
            tr d = (\(a,b,c,_,_) -> (a,b,c)) d
        f1 vs = foldl f2 (V.empty, V.empty, V.empty) vs
        f2 (ps,ns,us) (p,n,u) =
            ( V.snoc ps p
            , V.snoc ns n
            , V.snoc us u
            )

main :: IO ()
main = do


    args <- getArgs
    let level = case args of
            x:_ -> fromString x
            _ -> "L1S1.rfl"
        rfloc = case args of
            _:x:_ -> x
            _ -> "/Volumes/ML1/RedFaction/"

    let toLoad = map (rfloc ++) baseVPPs

    vpps <- foldM addVPP emptyVPPData toLoad

    Just rfl <- loadMap vpps level

    Just pipelineDesc <- decodeStrict <$> SB.readFile "render.json"

    win <- initWindow "Red Render" 640 640

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V4F
            "normal"    @: Attribute_V4F
            "uvw"       @: Attribute_V3F
          defUniforms $ do
            "time"      @: Float
            "meshTransform" @: M44F
            "diffuse"   @: FTexture2D

    storage <- LambdaCubeGL.allocStorage inputSchema

    let checkerImage = Juicy.ImageRGB8 $ Juicy.generateImage (\x y ->
            if mod (x + y) 2 == 0
                then Juicy.PixelRGB8 0 0 0
                else Juicy.PixelRGB8 255 255 0
            ) 2 2

    checkerTex <- LambdaCubeGL.uploadTexture2DToGPU' False False False False checkerImage

    tf <- forM (rflSections rfl) $ \case
        (_, RFLSectSG rooms) -> return $ Just $ roomsToMeshes rooms
        _ -> return Nothing

    texes <- foldM (loadTextures vpps checkerTex) M.empty tf

    obsL <- forM tf $ \case
        Just tris -> forM tris (loadMeshes storage texes)
        Nothing -> return []

    let obs = concat obsL

    putStrLn ("Objects: " ++ show (length obs))

    let st = (0)
    -- allocate GL pipeline
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc
    LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
      Just err -> putStrLn err
      Nothing  -> loop st
        where loop (state) = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= do
                              Just t <- GLFW.getTime
                              return (realToFrac t :: Float)
                forM_ (zip obs [1..]) $ \((r,obj), x) -> LambdaCubeGL.updateObjectUniforms obj $ do
                    let t = V3 ((sin (state + (fromIntegral r))) * 30) 0 ((sin (state * 0.64 + (fromIntegral r))) * 30)
                        o = quatOfEuler321 $ V3 0 (state * 0.05) 0 -- + (fromIntegral r))
                        s = V4 1 1 1 1
                        g = GeneralTransform s t o
                    --
                    "meshTransform" @= return (trans g)


                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                if escape then return () else loop (state + 0.05)

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
