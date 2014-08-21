{-# LANGUAGE RecordWildCards #-}
module Conics
  ( Bezier(..)
  , Conic(..)
  , bezierToConic
  , evalConic
  ) where

import Graphics.Gloss.Data.Vector

data Bezier =
  Bezier
  { bezierP0 :: Vector
  , bezierP1 :: Vector
  , bezierP2 :: Vector
  , bezierW  :: Float
  }

data Conic =
  Conic
  { conicAd :: Vector
  , conicAs :: Float
  , conicN  :: Vector
  , conicD  :: Float
  }

bezierToConic :: Bezier -> Conic
bezierToConic Bezier{..} =
  Conic
  { conicAd = a_d
  , conicAs = a_s
  , conicN  = n
  , conicD  = d
  }
  -- TODO: make it work for bezierP1 /= (0, 0)
  -- TODO: make it work for bezierW /= 1
 where
  p0 = bezierP0 - bezierP1
  p2 = bezierP2 - bezierP1
  (x0, y0) = p0
  (x2, y2) = p2
  xs = x0 + x2
  ys = y0 + y2
  a_d = (ys ^ 2, xs ^ 2)
  a_s = -xs * ys
  n = mulSV (2 * det) $ perp $ p2 - p0
  d = det ^ 2
  det = detV p0 p2

perp :: Vector -> Vector
perp (x, y) = (-y, x)

evalConic :: Conic -> Vector -> Float
evalConic Conic{..} p =
  conicD + dotV conicN p + normH conicAd conicAs p

-- multiply symmetric 2x2-matrix by 2-vector
mulHV :: Vector -> Float -> Vector -> Vector
mulHV (a, c) b (x, y) =
  (a * x + b * y, b * x + c * y)

normH :: Vector -> Float -> Vector -> Float
normH a_d a_s v = dotV v (mulHV a_d a_s v)
