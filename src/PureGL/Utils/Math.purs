module PureGL.Utils.Math where

import Prelude

import Math (abs, pi)

toRadians :: Number -> Number
toRadians x = x * (pi / 180.0)

class ApproxEq a where
  approxEq :: a -> a -> Boolean

infix 4 approxEq as ~=

instance approxEqNumber :: ApproxEq Number where
  approxEq x y = abs (x - y) <= 0.00000001
