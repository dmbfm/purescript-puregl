module Test.PureGL.Math.Vector.Fast where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import PureGL.Math.Vector (mkVector2, mkVector3, mkVector4)
import PureGL.Math.Vector.Fast as FV
import PureGL.WebGL.Types (WebGLEffRows)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

vectorFastSpec :: forall r. Spec (WebGLEffRows r) Unit
vectorFastSpec = describe "Vector.Fast" do
  describe "FVector2"do    
    it "Addition" do      
      res <- liftEff $ do
           a <- FV.mkFVector2 1.0 2.0
           b <- FV.mkFVector2 3.0 4.0
           FV.add a b b
           FV.toVector b
      shouldEqual res (mkVector2 4.0 6.0)
    
    it "Subtraction" do      
      res <- liftEff $ do
           a <- FV.mkFVector2 1.0 2.0
           b <- FV.mkFVector2 3.0 4.0
           FV.sub a b b
           FV.toVector b
      shouldEqual res (mkVector2 (-2.0) (-2.0))

    it "Inverse" do      
      res <- liftEff $ do
           a <- FV.mkFVector2 1.0 2.0           
           FV.inv a a 
           FV.toVector a
      shouldEqual res (mkVector2 (-1.0) (-2.0))

    it "Scalar Multiplication" do      
      res <- liftEff $ do
           a <- FV.mkFVector2 1.0 2.0
           FV.mul 2.0 a a
           FV.toVector a
      shouldEqual res (mkVector2 2.0 4.0)

    it "Dot Product" do      
      res <- liftEff $ do
           a <- FV.mkFVector2 1.0 2.0
           b <- FV.mkFVector2 3.0 4.0
           FV.dot a b
      shouldEqual res 11.0

  describe "FVector3" do    
    it "Addition" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 2.0 5.0
           b <- FV.mkFVector3 3.0 4.0 5.0
           FV.add a b b
           FV.toVector b
      shouldEqual res (mkVector3 4.0 6.0 10.0)
    
    it "Subtraction" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 2.0 5.0
           b <- FV.mkFVector3 3.0 4.0 5.0
           FV.sub a b b
           FV.toVector b
      shouldEqual res (mkVector3 (-2.0) (-2.0) 0.0)

    it "Inverse" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 2.0 5.0           
           FV.inv a a 
           FV.toVector a
      shouldEqual res (mkVector3 (-1.0) (-2.0) (-5.0))

    it "Scalar Multiplication" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 2.0 5.0
           FV.mul 2.0 a a
           FV.toVector a
      shouldEqual res (mkVector3 2.0 4.0 10.0)

    it "Dot Product" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 2.0 5.0
           b <- FV.mkFVector3 3.0 4.0 5.0
           FV.dot a b
      shouldEqual res 36.0

    it "Cross Product" do      
      res <- liftEff $ do
           a <- FV.mkFVector3 1.0 0.0 0.0
           b <- FV.mkFVector3 0.0 1.0 0.0
           FV.cross a b b
           FV.toVector b
      shouldEqual res (mkVector3 0.0 0.0 1.0)

  describe "FVector4" do    
    it "Addition" do      
      res <- liftEff $ do
           a <- FV.mkFVector4 1.0 2.0 5.0 6.0
           b <- FV.mkFVector4 3.0 4.0 5.0 6.0
           FV.add a b b
           FV.toVector b
      shouldEqual res (mkVector4 4.0 6.0 10.0 12.0)
    
    it "Subtraction" do      
      res <- liftEff $ do
           a <- FV.mkFVector4 1.0 2.0 5.0 6.0
           b <- FV.mkFVector4 3.0 4.0 5.0 6.0
           FV.sub a b b
           FV.toVector b
      shouldEqual res (mkVector4 (-2.0) (-2.0) 0.0 0.0)

    it "Inverse" do      
      res <- liftEff $ do
           a <- FV.mkFVector4 1.0 2.0 5.0 6.0           
           FV.inv a a 
           FV.toVector a
      shouldEqual res (mkVector4 (-1.0) (-2.0) (-5.0) (-6.0))

    it "Scalar Multiplication" do      
      res <- liftEff $ do
           a <- FV.mkFVector4 1.0 2.0 5.0 6.0
           FV.mul 2.0 a a
           FV.toVector a
      shouldEqual res (mkVector4 2.0 4.0 10.0 12.0)

    it "Dot Product" do      
      res <- liftEff $ do
           a <- FV.mkFVector4 1.0 2.0 5.0 6.0
           b <- FV.mkFVector4 3.0 4.0 5.0 6.0
           FV.dot a b
      shouldEqual res 72.0