module Test.PureGL.Math.Vector.Fast where

import Prelude

import Control.Monad.ST (newSTRef, pureST, readSTRef)
import PureGL.Math.Vector (dot)
import PureGL.Math.Vector.Fast as FV
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

vectorFastSpec :: forall r. Spec r Unit
vectorFastSpec = describe "Vector.Fast" do
  describe "FVector2"do
    let a = FV.mkFVector2 1.0 2.0
    let b = FV.mkFVector2 3.0 4.0
    let out = FV.mkFVector2 0.0 0.0
    it "Addition" do
      let x = pureST do
            res <- newSTRef out
            FV.add a b res
            readSTRef res
      shouldEqual x (FV.mkFVector2 4.0 6.0)

    it "Subtraction" do
      let x = pureST do
            res <- newSTRef out
            FV.sub a b res
            readSTRef res
      shouldEqual x (FV.mkFVector2 (-2.0) (-2.0))

    it "Inverse" do 
      let x = pureST do
            res <- newSTRef out
            FV.inv a res            
            FV.add a out res
            readSTRef res
      shouldEqual x (FV.mkFVector2 0.0 0.0)

    it "Scalar Multiplication" do
      let x = pureST do
            res <- newSTRef out
            FV.mul 2.0 a res
            readSTRef res
      shouldEqual x (FV.mkFVector2 2.0 4.0)

    it "Dot Product" do
      shouldEqual (dot a b) 11.0

  describe "FVector3" do
    let a = FV.mkFVector3 1.0 2.0 5.0
    let b = FV.mkFVector3 3.0 4.0 6.0
    let out = FV.mkFVector3 0.0 0.0 0.0
    it "Addition" do
      let x = pureST do
            res <- newSTRef out
            FV.add a b res
            readSTRef res
      shouldEqual x (FV.mkFVector3 4.0 6.0 11.0)

    it "Subtraction" do
      let x = pureST do
            res <- newSTRef out
            FV.sub a b res
            readSTRef res
      shouldEqual x (FV.mkFVector3 (-2.0) (-2.0) (-1.0))

    it "Inverse" do 
      let x = pureST do
            res <- newSTRef out
            FV.inv a res            
            FV.add a out res
            readSTRef res
      shouldEqual x (FV.mkFVector3 0.0 0.0 0.0)

    it "Scalar Multiplication" do
      let x = pureST do
            res <- newSTRef out
            FV.mul 2.0 a res
            readSTRef res
      shouldEqual x (FV.mkFVector3 2.0 4.0 10.0)

    it "Dot Product" do
      shouldEqual (dot a b) 41.0


  describe "FVector4" do
    let a = FV.mkFVector4 1.0 2.0 5.0 7.0
    let b = FV.mkFVector4 3.0 4.0 6.0 8.0
    let out = FV.mkFVector4 0.0 0.0 0.0 0.0
    it "Addition" do
      let x = pureST do
            res <- newSTRef out
            FV.add a b res
            readSTRef res
      shouldEqual x (FV.mkFVector4 4.0 6.0 11.0 15.0)

    it "Subtraction" do
      let x = pureST do
            res <- newSTRef out
            FV.sub a b res
            readSTRef res
      shouldEqual x (FV.mkFVector4 (-2.0) (-2.0) (-1.0) (-1.0))

    it "Inverse" do 
      let x = pureST do
            res <- newSTRef out
            FV.inv a res            
            FV.add a out res
            readSTRef res
      shouldEqual x (FV.mkFVector4 0.0 0.0 0.0 0.0)

    it "Scalar Multiplication" do
      let x = pureST do
            res <- newSTRef out
            FV.mul 2.0 a res
            readSTRef res
      shouldEqual x (FV.mkFVector4 2.0 4.0 10.0 14.0)

    it "Dot Product" do
      shouldEqual (dot a b) 97.0