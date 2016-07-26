{-# LANGUAGE RecordWildCards #-}
module Graphics.Math where
--

import LambdaCube.Linear
import qualified Data.Vect as V
import Data.Vect((.*.))

translate4 :: V.Vec3 -> V.Mat4
translate4 (V.Vec3 x y z) = V.Mat4
    (V.Vec4 1 0 0 0)
    (V.Vec4 0 1 0 0)
    (V.Vec4 0 0 1 0)
    (V.Vec4 x y z 1)

orient4 :: V.Vec4 -> V.Mat4
orient4 (V.Vec4 w x y z) = V.Mat4 q1 q2 q3 q4 where
    q1 = V.Vec4 (1 - 2 * y * y - 2 * z * z)
            (0 + 2 * x * y - 2 * z * w)
            (0 + 2 * x * z + 2 * y * w)
            0
    q2 = V.Vec4 (0 + 2 * x * y + 2 * z * w)
            (1 - 2 * x * x - 2 * z * z)
            (0 - 2 * y * z - 2 * x * w)
            0
    q3 = V.Vec4 (0 + 2 * x * z - 2 * y * w)
            (0 + 2 * y * z + 2 * x * w)
            (1 - 2 * x * x - 2 * y * y)
            0
    q4 = V.Vec4 0 0 0 1

scale4 :: V.Vec4 -> V.Mat4
scale4 (V.Vec4 x y z w) = V.Mat4
    (V.Vec4 x 0 0 0)
    (V.Vec4 0 y 0 0)
    (V.Vec4 0 0 z 0)
    (V.Vec4 0 0 0 w)


data GeneralTransform = GeneralTransform
    { genTransformScale :: V4 Float
    , genTransformTranslation :: V3 Float
    , genTransformOrient :: V4 Float
    } deriving (Show)

mat4ToV44 :: V.Mat4 -> V4 (V4 Float)
mat4ToV44 (V.Mat4 (V.Vec4 a b c d) (V.Vec4 e f g h) (V.Vec4 i j k l) (V.Vec4 m n o p))
    = V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)

v4ToVec4 :: V4 Float -> V.Vec4
v4ToVec4 (V4 x y z w) = V.Vec4 x y z w

v3ToVec3 :: V3 Float -> V.Vec3
v3ToVec3 (V3 x y z) = V.Vec3 x y z

-- Taken from SpacialMath library
quatOfEuler321 :: V3 Float -> V4 Float
quatOfEuler321 (V3 yaw pitch roll) = normalize q
  where
    sr2 = sin $ 0.5 * roll
    cr2 = cos $ 0.5 * roll
    sp2 = sin $ 0.5 * pitch
    cp2 = cos $ 0.5 * pitch
    sy2 = sin $ 0.5 * yaw
    cy2 = cos $ 0.5 * yaw
    qw = cr2*cp2*cy2 - sr2*sp2*sy2
    qy = sr2*cp2*cy2 + cr2*sp2*sy2
    qz = cr2*sp2*cy2 - sr2*cp2*sy2
    qx = cr2*cp2*sy2 + sr2*sp2*cy2
    q' = V4 qw qx qy qz
    q
      | qw < 0 = V4 (-qw) (-qx) (-qy) (-qz)
      | otherwise = q'

normalize :: V4 Float -> V4 Float
normalize (V4 w x y z) = V4 (w / sq) (x / sq) (y / sq) (z / sq) where
    sq = sqrt (w * w + x * x + y * y + z * z)

trans :: GeneralTransform -> V4 (V4 Float)
trans GeneralTransform{..} = mat4ToV44 mres where
    mres = orient4 (v4ToVec4 genTransformOrient)
        .*. scale4 (v4ToVec4 genTransformScale)
        .*. translate4 (v3ToVec3 genTransformTranslation)

    {-mres = translate4 (v3ToVec3 genTransformTranslation)
        .*. scale4 (v4ToVec4 genTransformScale)
        .*. orient4 (v4ToVec4 genTransformOrient)
    -}
