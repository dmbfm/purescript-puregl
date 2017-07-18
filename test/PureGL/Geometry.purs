module Test.PureGL.Geometry where

import Prelude

import PureGL.Geometry (attr3P3N2UV, attributeOffsets, vertexByteSize)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

geometrySpec :: forall r. Spec r Unit
geometrySpec = 
  describe "Geometry" do
    it "Vertex Byte Size" do
      shouldEqual (vertexByteSize attr3P3N2UV) 32
    it "Attribute offsets" do
      shouldEqual (attributeOffsets attr3P3N2UV) [0, 12, 24]
