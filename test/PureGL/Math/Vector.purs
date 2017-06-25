module Test.PureGL.Math.Vector where

import Prelude

import PureGL.Math.Vector as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

vectorSpec :: forall r. Spec r Unit
vectorSpec = describe "Vector" do
  describe "Vector2" do
    it "Addition" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      let v2 = V.Vector2 {x: 3.0, y: 4.0}
      let r  = V.Vector2 {x: 4.0, y: 6.0}
      shouldEqual (V.add v1 v2) r
    it "Subtraction" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      let v2 = V.Vector2 {x: 3.0, y: 4.0}
      let r  = V.Vector2 {x: -2.0, y: -2.0}
      shouldEqual (V.sub v1 v2) r
    it "Zero" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      shouldEqual (V.add v1 V.zero) v1
    it "Inverse" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      shouldEqual (V.add v1 (V.inv v1)) V.zero
    it "Scalar multiplication" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      let r  = V.Vector2 {x: 2.0, y: 4.0}
      let a = 2.0
      shouldEqual (V.mul a v1) r
    it "Dot product" do
      let v1 = V.Vector2 {x: 1.0, y: 2.0}
      let v2 = V.Vector2 {x: 3.0, y: 4.0}
      let r = 11.0
      shouldEqual (V.dot v1 v2) r

  describe "Vector3" do
    it "Addition" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 5.0}
      let v2 = V.Vector3 {x: 3.0, y: 4.0, z: 6.0}
      let r  = V.Vector3 {x: 4.0, y: 6.0, z: 11.0}
      shouldEqual (V.add v1 v2) r
    it "Subtraction" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 5.0}
      let v2 = V.Vector3 {x: 3.0, y: 4.0, z: 6.0}
      let r  = V.Vector3 {x: -2.0, y: -2.0, z: -1.0}
      shouldEqual (V.sub v1 v2) r
    it "Zero" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 3.0}
      shouldEqual (V.add v1 V.zero) v1
    it "Inverse" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 3.0}
      shouldEqual (V.add v1 (V.inv v1)) V.zero
    it "Scalar multiplication" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 5.0}
      let r  = V.Vector3 {x: 2.0, y: 4.0, z: 10.0}
      let a = 2.0
      shouldEqual (V.mul a v1) r
    it "Dot product" do
      let v1 = V.Vector3 {x: 1.0, y: 2.0, z: 5.0}
      let v2 = V.Vector3 {x: 3.0, y: 4.0, z: 6.0}
      let r = 41.0
      shouldEqual (V.dot v1 v2) r
    it "Cross product" do
      let i = V.Vector3 {x: 1.0, y: 0.0, z: 0.0}
      let j = V.Vector3 {x: 0.0, y: 1.0, z: 0.0}
      let k = V.Vector3 {x: 0.0, y: 0.0, z: 1.0}
      shouldEqual (V.cross i j) k
      shouldEqual (V.cross k i) j
      shouldEqual (V.cross j k) i

  describe "Vector4" do
    it "Addition" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 5.0, w: 7.0}
      let v2 = V.Vector4 {x: 3.0, y: 4.0, z: 6.0, w: 8.0}
      let r  = V.Vector4 {x: 4.0, y: 6.0, z: 11.0, w: 15.0}
      shouldEqual (V.add v1 v2) r
    it "Subtraction" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 5.0, w: 7.0}
      let v2 = V.Vector4 {x: 3.0, y: 4.0, z: 6.0, w: 8.0}
      let r  = V.Vector4 {x: -2.0, y: -2.0, z: -1.0, w: -1.0}
      shouldEqual (V.sub v1 v2) r
    it "Zero" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 3.0, w: 4.0}
      shouldEqual (V.add v1 V.zero) v1
    it "Inverse" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 3.0, w: 4.0}
      shouldEqual (V.add v1 (V.inv v1)) V.zero
    it "Scalar multiplication" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 5.0, w: 6.0}
      let r  = V.Vector4 {x: 2.0, y: 4.0, z: 10.0, w: 12.0}
      let a = 2.0
      shouldEqual (V.mul a v1) r
    it "Dot product" do
      let v1 = V.Vector4 {x: 1.0, y: 2.0, z: 5.0, w: 7.0}
      let v2 = V.Vector4 {x: 3.0, y: 4.0, z: 6.0, w: 8.0}
      let r = 97.0
      shouldEqual (V.dot v1 v2) r