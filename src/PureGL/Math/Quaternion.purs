module PureGL.Math.Quaternion where

import Prelude

import Math (abs, cos, sin, sqrt)
import PureGL.Math.Vector as V
import PureGL.Utils.Math (class ApproxEq, approxEq, toRadians, (~=))

newtype Quaternion = Quaternion { a:: Number, b :: Number, c :: Number, d :: Number }

-- | Create a `Quaternion` from given numbers.
mkQuaternion :: Number -> Number -> Number -> Number -> Quaternion
mkQuaternion a b c d = Quaternion { a: a, b: b, c: c, d: d}

fromVector3 :: Number -> V.Vector3 -> Quaternion
fromVector3 a (V.Vector3 v) = Quaternion { a: a
                                         , b: v.x
                                         , c: v.y
                                         , d: v.z
                                         }

fromVector4 :: V.Vector4 -> Quaternion
fromVector4 (V.Vector4 v) = Quaternion { a: v.x
                                       , b: v.y
                                       , c: v.z
                                       , d: v.w
                                       }

realPart :: Quaternion -> Number
realPart (Quaternion q) = q.a

imaginaryPart :: Quaternion -> V.Vector3
imaginaryPart (Quaternion q) = V.mkVector3 q.b q.c q.d

fromAxisRotation :: Number -> V.Vector3 -> Quaternion
fromAxisRotation t v = 
  Quaternion  { a: c
              , b: u.x * s
              , c: u.y * s
              , d: u.z * s
              }
  where
    c = cos $ toRadians $ t / 2.0
    s = sin $ toRadians $ t / 2.0
    (V.Vector3 u) = V.normalize v

instance semiRingQuaternion :: Semiring Quaternion where
  one = mkQuaternion 1.0 0.0 0.0 0.0
  mul = mulQuaternion
  zero = mkQuaternion 0.0 0.0 0.0 0.0
  add = addQ 

instance ringQuaternion :: Ring Quaternion where
  sub = subQ

instance vectorQuaternion :: V.Vector Quaternion where
  add = addQ
  sub = subQ
  zero = mkQuaternion 0.0 0.0 0.0 0.0
  inv = negQ
  mul = mulQ

instance innerProductQuaternion :: V.InnerProduct Quaternion where
  dot (Quaternion q1) (Quaternion q2) = 
    q1.a * q2.a + q1.b * q2.b + q1.c * q2.c + q1.d * q2.d

instance divisionRingQuaternion :: DivisionRing Quaternion where
  recip q = V.mul (1.0 / ( V.dot q q)) (conjugate q)

instance eqQuaternion :: Eq Quaternion where
  eq (Quaternion q1) (Quaternion q2) = 
    q1.a == q2.a &&
    q1.b == q2.b &&
    q1.c == q2.c &&
    q1.d == q2.d 

instance approxEqQuaternion :: ApproxEq Quaternion where
  approxEq (Quaternion q1) (Quaternion q2) = 
    q1.a ~= q2.a &&
    q1.b ~= q2.b &&
    q1.c ~= q2.c &&
    q1.d ~= q2.d 

instance showQuaternion :: Show Quaternion where
  show (Quaternion q1) = "(" <> show (q1.a) <> " + "
                             <> show (q1.b) <> "i + " 
                             <> show (q1.c) <> "j + "
                             <> show (q1.d) <> "k)" 
                             
conjugate :: Quaternion -> Quaternion
conjugate (Quaternion q) = 
  Quaternion { a:  q.a 
             , b: -q.b
             , c: -q.c
             , d: -q.d  
             }

addQ :: Quaternion -> Quaternion -> Quaternion
addQ (Quaternion q1) (Quaternion q2) =
  Quaternion { a: q1.a + q2.a
              , b: q1.b + q2.b
              , c: q1.c + q2.c
              , d: q1.d + q2.d 
              }
subQ :: Quaternion -> Quaternion -> Quaternion
subQ (Quaternion q1) (Quaternion q2) =
  Quaternion { a: q1.a - q2.a
              , b: q1.b - q2.b
              , c: q1.c - q2.c
              , d: q1.d - q2.d 
              }
zeroQ :: Quaternion
zeroQ = Quaternion{ a: 0.0, b: 0.0, c: 0.0, d: 0.0 }

negQ :: Quaternion -> Quaternion
negQ (Quaternion q1) = 
  Quaternion { a: -q1.a
              , b: -q1.b
              , c: -q1.c
              , d: -q1.d
              }

mulQ :: Number -> Quaternion -> Quaternion
mulQ a (Quaternion q) = 
  Quaternion { a: a * q.a
             , b: a * q.b
             , c: a * q.c
             , d: a * q.d
             }

mulQuaternion :: Quaternion -> Quaternion -> Quaternion
mulQuaternion (Quaternion q1) (Quaternion q2) = 
  Quaternion { a: q1.a * q2.a - q1.b * q2.b - q1.c * q2.c - q1.d * q2.d 
             , b: q1.a * q2.b + q1.b * q2.a + q1.c * q2.d - q1.d * q2.c
             , c: q1.a * q2.c - q1.b * q2.d + q1.c * q2.a + q1.d * q2.b
             , d: q1.a * q2.d + q1.b * q2.c - q1.c * q2.b + q1.d * q2.a
             }

rotateVector :: Quaternion -> V.Vector3 -> V.Vector3
rotateVector q v = imaginaryPart $ q * (fromVector3 0.0 v) * (recip q)

fromTwoVectors :: V.Vector3 -> V.Vector3 -> Quaternion
fromTwoVectors u@(V.Vector3 u') v@(V.Vector3 v') = 
  let x = sqrt $ (V.dot u u) * (V.dot v v)
      r = x + (V.dot u v)
      w = if ((abs u'.x) > (abs u'.z)) 
            then (V.mkVector3 (-u'.y) u'.x 0.0) 
            else  (V.mkVector3 0.0 (-u'.z) u'.y)
  in 
    if (r < (0.000001 * x))
      then 
        V.normalize $ fromVector3 0.0 w
      else 
        V.normalize $ fromVector3 r (V.cross u v)


lookAt :: V.Vector3 -> V.Vector3 -> V.Vector3 -> Quaternion
lookAt eye point up = 
  let z' = V.sub eye point
      x' = V.cross up z'
      y' = V.cross z' x'
      q1 = fromTwoVectors (V.mkVector3 0.0 0.0 1.0) z' 
      newUp = rotateVector q1 (V.mkVector3 0.0 1.0 0.0)
      q2 = fromTwoVectors newUp y'
  in q2 * q1
