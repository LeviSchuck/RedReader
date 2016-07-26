module Graphics.Mesh where
--
import qualified Data.Vector as V
import qualified Data.Map as M
--
import LambdaCube.GL as LambdaCubeGL -- renderer

import Graphics.Math

data SubMesh = SubMesh
    { subMeshTriPositions :: V.Vector (V4 Float)
    , subMeshTriNormals :: V.Vector (V4 Float)
    , subMeshTriUVs :: V.Vector (V3 Float)
    , subMeshTexture :: Int
    -- TODO: lightmap stuff
    }

data Mesh = Mesh
    { meshSubMeshes :: V.Vector SubMesh
    }

data MeshSet = MeshSet
    { meshes :: M.Map Int Mesh
    , meshTransform :: GeneralTransform
    }
