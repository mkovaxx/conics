{-# LANGUAGE RecordWildCards #-}
module Conics
  ( Bezier(..)
  , Conic(..)
  , bezierToConic
  , evalConic
  ) where

import Graphics.Gloss.Data.Vector

import Util

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
  , conicN  = n - mulSV 2 (mulHV a_d a_s p1)
  , conicD  = d - dotV n p1 + normHV a_d a_s p1
  }
 where
  p0 = bezierP0 - bezierP1
  p2 = bezierP2 - bezierP1
  p1 = bezierP1
  ws = bezierW ^ 2
  (x0, y0) = p0
  (x2, y2) = p2
  pd = p2 - p0
  (xd, yd) = pd
  det = detV p0 p2
  a_d = (yd ^ 2 + 4.0 * ws * y0 * y2,
         xd ^ 2 + 4.0 * ws * x0 * x2)
  a_s = -(xd * yd + 2.0 * ws * (x0 * y2 + x2 * y0))
  n = mulSV (2.0 * det) $ rotV pd
  d = det ^ 2

evalConic :: Conic -> Vector -> Float
evalConic Conic{..} p =
  conicD + dotV conicN p + normHV conicAd conicAs p
