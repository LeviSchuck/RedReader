module Graphics.Textures where
--
import qualified Data.ByteString as B
import qualified Data.Map as M
import Codec.Picture as Juicy
import LambdaCube.GL

--

data LoadedTextures = LoadedTextures
    { nextTexIndex :: Int
    , loadedImages :: M.Map Int DynamicImage
    , loadedReferences :: M.Map Int Int
    , namedReferences :: M.Map B.ByteString Int
    , errorTexture :: Int
    }

defaultLoadedTextures = LoadedTextures 0 M.empty M.empty M.empty 0

data UploadedTextures = UploadedTextures
    { uploadedGPUTextures :: M.Map Int TextureData
    }

defaultUploadedTExtures = UploadedTextures M.empty

--

type LoadTextureFn :: B.ByteString -> IO (Maybe B.ByteString)
