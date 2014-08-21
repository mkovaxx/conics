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
 where
  p0 = bezierP0 - bezierP1
  p2 = bezierP2 - bezierP1
  dif = p0 - p2
  det = detV p0 p2
  a_d = undefined
  a_s = undefined
  -- n = mulSV (2 * det) dif
  -- d = det ^ 2
  n = cross $ bezierP2 - bezierP0
  d = (-1) * dotV bezierP0 n

cross :: Vector -> Vector
cross (x, y) = (-y, x)

evalConic :: Conic -> Vector -> Float
evalConic Conic{..} p =
  conicD + dotV conicN p

normH :: Vector -> Float -> Vector -> Float
normH (a, c) b (x, y) =
  (a * x ^ 2) + (2 * b * x * y) + (c * y ^ 2)
