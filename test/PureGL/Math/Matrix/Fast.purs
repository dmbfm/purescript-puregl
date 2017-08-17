module Test.PureGL.Math.Matrix.Fast where

import Prelude

import Control.Monad.ST (newSTRef, pureST, readSTRef)
import PureGL.Math.Matrix.Fast as MF
import PureGL.Math.Vector.Fast as VF
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

matrixFastSpec :: forall r. Spec r Unit
matrixFastSpec = 
  describe "Matrix.Fast" do
    describe "FMatrix2" do
      let a = MF.mkFMatrix2 1.0 2.0 3.0 4.0
      let b = MF.mkFMatrix2 5.0 6.0 7.0 8.0
      let out  = (VF.zero unit) :: MF.FMatrix2
      it "Addition" do
        let x = pureST do
              res <- newSTRef out
              VF.add a b res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 6.0 8.0 10.0 12.0)
      it "Subtraction" do
        let x = pureST do
              res <- newSTRef out
              VF.sub b a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 4.0 4.0 4.0 4.0)
      it "Zero" do
        let x = pureST do
              res <- newSTRef out
              VF.add a (VF.zero unit) res
              readSTRef res
        shouldEqual x a
      it "Arithmetic inverse" do
        let x = pureST do
              res <- newSTRef out
              VF.inv a res
              z <- readSTRef res
              VF.add a z res
              readSTRef res
        shouldEqual x (VF.zero unit)
      it "Scalar Multiplication" do
        let x = pureST do
              res <- newSTRef out
              VF.mul 2.0 a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 2.0 4.0 6.0 8.0)
      it "Identity" do
        let x = pureST do
              res <- newSTRef out
              VF.add a (MF.identity unit) res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 2.0 2.0 3.0 5.0)
      it "Transpose" do
        let x = pureST do
              res <- newSTRef out
              MF.transpose a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 1.0 3.0 2.0 4.0)
      it "Matrix Multiplication" do
        let x = pureST do
              res <- newSTRef out
              MF.mulMatrix a b res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 19.0 22.0 43.0 50.0)
      it "Determinant" do
        let m1 = MF.mkFMatrix2 1.0 2.0 3.0 4.0
        let r = (-2.0)
        shouldEqual (MF.determinant m1) r
      it "Matrix Inverse" do
        let x = pureST do
              res <- newSTRef out
              _ <- MF.invert a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix2 (-2.0) 1.0 (3.0 / 2.0) (-(1.0 / 2.0)))
  
    describe "FMatrix3" do
      let a = MF.mkFMatrix3 1.0 2.0 3.0 
                            4.0 5.0 6.0
                            7.0 8.0 9.0
      let b = MF.mkFMatrix3 10.0 11.0 12.0
                            13.0 14.0 15.0
                            16.0 17.0 18.0
      let m = MF.mkFMatrix3 1.0 0.0 3.0
                             4.0 1.0 0.0
                             0.0 1.0 1.0                            
      let out  = (VF.zero unit) :: MF.FMatrix3
      it "Addition" do
        let x = pureST do
              res <- newSTRef out
              VF.add a b res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 11.0 13.0 15.0
                                     17.0 19.0 21.0
                                     23.0 25.0 27.0)
      it "Subtraction" do
        let x = pureST do
              res <- newSTRef out
              VF.sub b a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 9.0 9.0 9.0
                                     9.0 9.0 9.0
                                     9.0 9.0 9.0)
      it "Zero" do
        let x = pureST do
              res <- newSTRef out
              VF.add a (VF.zero unit) res
              readSTRef res
        shouldEqual x a
      it "Arithmetic inverse" do
        let x = pureST do
              res <- newSTRef out
              VF.inv a res
              z <- readSTRef res
              VF.add a z res
              readSTRef res
        shouldEqual x (VF.zero unit)
      it "Scalar Multiplication" do
        let x = pureST do
              res <- newSTRef out
              VF.mul 2.0 a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 2.0 4.0 6.0
                                     8.0 10.0 12.0
                                     14.0 16.0 18.0)
      it "Identity" do
        let x = pureST do
              res <- newSTRef out
              VF.add a (MF.identity unit) res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 2.0 2.0 3.0 
                                     4.0 6.0 6.0
                                     7.0 8.0 10.0)
      it "Transpose" do
        let x = pureST do
              res <- newSTRef out
              MF.transpose a res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 1.0 4.0 7.0
                                     2.0 5.0 8.0
                                     3.0 6.0 9.0)
      it "Matrix Multiplication" do
        let x = pureST do
              res <- newSTRef out
              MF.mulMatrix a b res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 84.0 90.0 96.0
                                     201.0 216.0 231.0
                                     318.0 342.0 366.0)
      it "Determinant" do                       
        shouldEqual (MF.determinant m) 13.0

      it "Matrix Inverse" do
        let x = pureST do
              res <- newSTRef out
              _ <- MF.invert m res
              readSTRef res
        shouldEqual x (MF.mkFMatrix3 (1.0 / 13.0) (3.0 / 13.0) ((-3.0) / 13.0)
                                     ((-4.0) / 13.0) (1.0 / 13.0) (12.0 / 13.0)
                                     (4.0 / 13.0) ((-1.0) / 13.0) (1.0 / 13.0))
    