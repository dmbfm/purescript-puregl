module Test.PureGL.Math.Matrix where

import Prelude

import Data.Maybe (Maybe(..))
import PureGL.Math.Matrix (Matrix2, Matrix3, applyTransform, determinant, fromArray, identity, invert, mkMatrix2, mkMatrix3, mkMatrix4, mkRotateX, mkRotateY, mkRotateZ, mkRotation, mkScale, mkScale', mkTranslation, mkTranslation', mulMatrix, transpose)
import PureGL.Math.Vector (mkVector3, mkVector4)
import PureGL.Math.Vector as V
import PureGL.Utils.Math (approxEq)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

matrixSpec :: forall r. Spec r Unit
matrixSpec = describe "Matrix" do
  describe "Matrix2" do
    it "Addition" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let m2 = mkMatrix2 5.0 6.0 7.0 8.0
      let r  = mkMatrix2 6.0 8.0 10.0 12.0
      shouldEqual (V.add m1 m2) r
      
    it "Subtraction" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let m2 = mkMatrix2 5.0 6.0 7.0 8.0
      let r  = mkMatrix2 4.0 4.0 4.0 4.0
      shouldEqual (V.sub m2 m1) r

    it "Zero" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let m2 = V.zero
      shouldEqual (V.add m1 m2) m1

    it "Arithmetic Inverse" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      shouldEqual (V.add m1 (V.inv m1)) V.zero

    it "Scalar Multiplication" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let a = 2.0
      let r  = mkMatrix2 2.0 4.0 6.0 8.0
      shouldEqual (V.mul a m1) r

    it "Identity" do
      let m1 = identity :: Matrix2
      let m2 = mkMatrix2 1.0 2.0 3.0 4.0
      let r  = mkMatrix2 2.0 2.0 3.0 5.0
      shouldEqual (V.add m1 m2) r
    
    it "Transpose" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let r  = mkMatrix2 1.0 3.0 2.0 4.0
      shouldEqual (transpose m1) r

    it "Matrix Multiplication" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let m2 = mkMatrix2 5.0 6.0 7.0 8.0
      let r  = mkMatrix2 19.0 22.0 43.0 50.0
      shouldEqual (mulMatrix m1 m2) r

    it "Determinant" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let r = (-2.0)
      shouldEqual (determinant m1) r

    it "Matrix Inverse" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let r  = mkMatrix2 (-2.0) 1.0 (3.0 / 2.0) (-(1.0 / 2.0))
      case (invert m1) of
        Nothing -> shouldEqual 0 1
        Just r' -> shouldEqual r' r

    it "fromArray" do
      let m1 = mkMatrix2 1.0 2.0 3.0 4.0
      let m2 = fromArray [1.0, 2.0, 3.0, 4.0]
      shouldEqual m1 m2

  describe "Matrix3" do
    let m = mkMatrix3  1.0 2.0 3.0 
                       4.0 5.0 6.0
                       7.0 8.0 9.0
    let m' = mkMatrix3  10.0 11.0 12.0
                        13.0 14.0 15.0
                        16.0 17.0 18.0
    it "Addition" do
      let m1 = mkMatrix3  1.0 2.0 3.0 
                          4.0 5.0 6.0
                          7.0 8.0 9.0

      let m2 = mkMatrix3  10.0 11.0 12.0
                          13.0 14.0 15.0
                          16.0 17.0 18.0

      let r  = mkMatrix3 11.0 13.0 15.0
                         17.0 19.0 21.0
                         23.0 25.0 27.0 
      shouldEqual (V.add m1 m2) r
      
    it "Subtraction" do
      let m1 = mkMatrix3  1.0 2.0 3.0 
                          4.0 5.0 6.0
                          7.0 8.0 9.0

      let m2 = mkMatrix3  10.0 11.0 12.0
                          13.0 14.0 15.0
                          16.0 17.0 18.0

      let r  = mkMatrix3 9.0 9.0 9.0
                         9.0 9.0 9.0
                         9.0 9.0 9.0
      shouldEqual (V.sub m2 m1) r

    it "Zero" do
      let m1 = mkMatrix3  1.0 2.0 3.0 
                          4.0 5.0 6.0
                          7.0 8.0 9.0
      let m2 = V.zero
      shouldEqual (V.add m1 m2) m1

    it "Arithmetic Inverse" do
      let m1 = mkMatrix3  1.0 2.0 3.0 
                          4.0 5.0 6.0
                          7.0 8.0 9.0
      let r  = mkMatrix3
      shouldEqual (V.add m1 (V.inv m1)) V.zero

    it "Scalar Multiplication" do
      let m1 = mkMatrix3  1.0 2.0 3.0 
                          4.0 5.0 6.0
                          7.0 8.0 9.0
      let a = 2.0
      let r  = mkMatrix3 2.0 4.0 6.0
                         8.0 10.0 12.0
                         14.0 16.0 18.0
      shouldEqual (V.mul a m1) r

    it "Identity" do
      let m1 = identity :: Matrix3
      let r  = mkMatrix3 2.0 2.0 3.0 
                         4.0 6.0 6.0
                         7.0 8.0 10.0
      shouldEqual (V.add m1 m) r
    
    it "Transpose" do
      let r = mkMatrix3 1.0 4.0 7.0
                        2.0 5.0 8.0
                        3.0 6.0 9.0
      shouldEqual (transpose m) r
  
    it "Matrix Multiplication" do
      let r = mkMatrix3 84.0 90.0 96.0
                        201.0 216.0 231.0
                        318.0 342.0 366.0
      shouldEqual (mulMatrix m m') r

    it "Determinant" do
      let m1 = mkMatrix3 1.0 0.0 3.0
                         4.0 1.0 0.0
                         0.0 1.0 1.0
      let r = 13.0
      shouldEqual (determinant m1) r

    it "Matrix Inverse" do
      let m1 = mkMatrix3 1.0 0.0 3.0
                         4.0 1.0 0.0
                         0.0 1.0 1.0
      let r  = mkMatrix3 (1.0 / 13.0) (3.0 / 13.0) ((-3.0) / 13.0)
                         ((-4.0) / 13.0) (1.0 / 13.0) (12.0 / 13.0)
                         (4.0 / 13.0) ((-1.0) / 13.0) (1.0 / 13.0)
      case (invert m1) of
        Nothing -> shouldEqual 1 0
        Just r' -> shouldEqual r' r

    it "fromArray" do
      let m2 = fromArray [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
      shouldEqual m m2

  describe "Matrix4" do
    let m = mkMatrix4 1.0 0.0 0.0 1.0
                      0.0 2.0 1.0 2.0
                      2.0 1.0 0.0 1.0
                      2.0 0.0 1.0 4.0

    let m' = mkMatrix4 2.0  3.0  3.0  1.0
                       0.0  4.0  3.0 (-3.0)
                       2.0 (-1.0) (-1.0) (-3.0)
                       0.0 (-4.0) (-3.0)  2.0

    it "Addition" do
      let r = mkMatrix4 3.0  3.0  3.0  2.0
                        0.0  6.0  4.0 (-1.0)
                        4.0  0.0 (-1.0) (-2.0)
                        2.0 (-4.0) (-2.0)  6.0
      shouldEqual (V.add m m') r
    
    it "Subtraction" do
      let r = mkMatrix4 (-1.0) (-3.0) (-3.0) 0.0
                        0.0 (-2.0) (-2.0) 5.0
                        0.0 2.0 1.0 4.0
                        2.0 4.0 4.0 2.0
      shouldEqual (V.sub m m') r

    it "Zero" do
      shouldEqual (V.add m V.zero) m
    
    it "Arithmetic Inverse" do
      shouldEqual (V.add m (V.inv m)) V.zero

    it "Scalar Multiplication" do
      let r = mkMatrix4 2.0 0.0 0.0 2.0
                        0.0 4.0 2.0 4.0
                        4.0 2.0 0.0 2.0
                        4.0 0.0 2.0 8.0
      shouldEqual (V.mul 2.0 m) r

    it "Identity" do
      let r = mkMatrix4 2.0 0.0 0.0 1.0
                       0.0 3.0 1.0 2.0
                       2.0 1.0 1.0 1.0
                       2.0 0.0 1.0 5.0
      shouldEqual (V.add m identity) r

    it "Transpose" do
      let r = mkMatrix4 1.0 0.0 2.0 2.0
                        0.0 2.0 1.0 0.0
                        0.0 1.0 0.0 1.0
                        1.0 2.0 1.0 4.0
      shouldEqual (transpose m) r

    it "Matrix Multiplication" do
      let r = mkMatrix4 2.0 (-1.0) 0.0 3.0
                        2.0 (-1.0) (-1.0) (-5.0)
                        4.0 6.0 6.0 1.0
                        6.0 (-11.0) (-7.0) 7.0
      shouldEqual (mulMatrix m m') r

    it "Determinant" do
      shouldEqual (determinant m) 2.0
    
    it "Matrix Inverse" do
      let r = mkMatrix4 (-2.0) (-0.5) 1.0 0.5
                        1.0 0.5 0.0 (-0.5)
                        (-8.0) (-1.0) 2.0 2.0
                        3.0 0.5 (-1.0) (-0.5)
      case (invert m) of
        Nothing -> shouldEqual 1 0
        Just r' -> shouldEqual r' r

    it "fromArray" do
      let r = fromArray [ 1.0, 0.0, 0.0, 1.0
                        , 0.0, 2.0, 1.0, 2.0
                        , 2.0, 1.0, 0.0, 1.0
                        , 2.0, 0.0, 1.0, 4.0 ]
      shouldEqual m r

    it "mkTranslation / applyTransform" do
      let t = mkTranslation 1.0 1.0 1.0
      let x = applyTransform t (mkVector4 0.0 0.0 0.0 1.0)
      shouldEqual x (mkVector4 1.0 1.0 1.0 1.0)

    it "mkScale / applyTransform" do
      let t = mkScale 2.0 2.0 2.0
      let x = applyTransform t (mkVector4 1.0 1.0 1.0 1.0)
      shouldEqual x (mkVector4 2.0 2.0 2.0 1.0)

    it "mkRotateZ / applyTransform" do
      let t = mkRotateZ 90.0
      let x = applyTransform t (mkVector4 1.0 0.0 0.0 1.0)
      shouldEqual (approxEq x (mkVector4 0.0 1.0 0.0 1.0)) true
    
    it "mkRotateY / applyTransform" do
      let t = mkRotateY 90.0
      let x = applyTransform t (mkVector4 1.0 0.0 0.0 1.0)
      --shouldEqual x (mkVector4 2.0 2.0 2.0 1.0)
      shouldEqual (approxEq x (mkVector4 0.0 0.0 (-1.0) 1.0)) true

    it "mkRotateX / applyTransform" do
      let t = mkRotateX 90.0
      let x = applyTransform t (mkVector4 0.0 1.0 0.0 1.0)
      --shouldEqual x (mkVector4 2.0 2.0 2.0 1.0)
      shouldEqual (approxEq x (mkVector4 0.0 0.0 1.0 1.0)) true

    it "mkRotation / applyTransform" do
      let t1 = mkRotation (mkVector3 0.0 0.0 1.0) 90.0
      let t2 = mkRotation (mkVector3 0.0 1.0 0.0) 90.0
      let t3 = mkRotation (mkVector3 1.0 0.0 0.0) 90.0

      let x1 = applyTransform t1 (mkVector4 1.0 0.0 0.0 1.0)
      let x2 = applyTransform t2 (mkVector4 1.0 0.0 0.0 1.0)
      let x3 = applyTransform t3 (mkVector4 0.0 1.0 0.0 1.0)
      --shouldEqual x (mkVector4 2.0 2.0 2.0 1.0)
      shouldEqual (approxEq x1 (mkVector4 0.0 1.0 0.0 1.0)) true
      shouldEqual (approxEq x2 (mkVector4 0.0 0.0 (-1.0) 1.0)) true
      shouldEqual (approxEq x3 (mkVector4 0.0 0.0 1.0 1.0)) true