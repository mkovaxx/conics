module Util where

import Graphics.Gloss.Data.Vector

-- rotate vector by right angle CCW
rotV :: Vector -> Vector
rotV (x, y) = (-y, x)

-- multiply symmetric 2x2-matrix by 2-vector
mulHV :: Vector -> Float -> Vector -> Vector
mulHV (a, c) b (x, y) =
  (a * x + b * y, b * x + c * y)

-- <v|A|v> with symmetric A
normHV :: Vector -> Float -> Vector -> Float
normHV a_d a_s v = dotV v (mulHV a_d a_s v)
