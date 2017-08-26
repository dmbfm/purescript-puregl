module Test.PureGL.Math.Quaternion where

import Prelude

import Data.DivisionRing (leftDiv, rightDiv)
import PureGL.Math.Quaternion (conjugate, mkQuaternion)
import PureGL.Math.Vector as V
import PureGL.Utils.Math (approxEq)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

quaternionSpec :: forall r. Spec r Unit
quaternionSpec = describe "Quaternion" do

  let q1 = mkQuaternion 1.0 2.0 3.0 4.0
  let q2 = mkQuaternion 5.0 6.0 7.0 8.0

  it "add / zero" do
    shouldEqual (q1 + q2) (mkQuaternion 6.0 8.0 10.0 12.0)
    shouldEqual (q1 + zero) q1

  it "mul / one" do
    shouldEqual (q1 * q2) (mkQuaternion (-60.0) 12.0 30.0 24.0)
    shouldEqual (q1 * one) q1
    shouldEqual (one * q1) q1

  it "sub" do
    shouldEqual (q2 - q1) (mkQuaternion 4.0 4.0 4.0 4.0)

  it "V.mul" do
    shouldEqual (V.mul 2.0 q1) (mkQuaternion 2.0 4.0 6.0 8.0)

  it "dot" do
    shouldEqual (V.dot q1 q2) (5.0 + 2.0 * 6.0 + 3.0 * 7.0 + 4.0 * 8.0)

  it "recip / division" do
    shouldEqual (approxEq (leftDiv q1 q1) one) true
    shouldEqual (approxEq (rightDiv q1 q1) one) true

  it "conjugate" do
    shouldEqual (conjugate q1) (mkQuaternion 1.0 (-2.0) (-3.0) (-4.0))
    shouldEqual ((conjugate q1) * (conjugate q2)) (conjugate $ q2 * q1)
    