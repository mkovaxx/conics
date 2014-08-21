{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Raster.Field

import Conic
import Util

knobRadius :: Float
knobRadius = 6

main :: IO ()
main = do
  let
    position = (10, 10)
  play
    (InWindow "Conic Sections and Quadratic Rational Bezier Curves" (size initialize) position)
    white
    0
    initialize
    draw
    input
    step

data State =
  State
  { size :: (Int, Int)
  , cursor :: (Float, Float)
  , point0 :: (Float, Float)
  , point1 :: (Float, Float)
  , point2 :: (Float, Float)
  , weight :: Float
  , selection :: Maybe Int
  }

initialize :: State
initialize =
  State
  { size = (800, 600)
  , cursor = (0, 0)
  , point0 = (40, 250)
  , point1 = (0, 0)
  , point2 = (200, 60)
  , weight = 1
  , selection = Nothing
  }

draw :: State -> Picture
draw State{..} =
  Pictures [plot, hud]
 where
  hud = Color black $ Pictures
    [ Line [point0, point1]
    , Line [point1, point2]
    , uncurry Translate point0 $ Circle knobRadius
    , uncurry Translate point1 $ Circle knobRadius
    , uncurry Translate point2 $ Circle knobRadius
    ]
  bezier =
    Bezier
    { bezierP0 = point0
    , bezierP1 = point1
    , bezierP2 = point2
    , bezierW  = weight
    }
  conic = bezierToConic bezier
  plot = plotConic size white violet conic

plotConic :: (Int, Int) -> Color -> Color -> Conic -> Picture
plotConic size positive negative conic =
  makePicture (fst size) (snd size) 1 1
    ( (\v -> if v > 0 then positive else negative)
    . evalConic conic
    . (\(x, y) -> (0.5 * fromIntegral (fst size) * x, 0.5 * fromIntegral (snd size) * y))
    )

input :: Event -> State -> State
input event state@State{..} = case event of
  EventResize newSize ->
    state{ size = newSize }
  EventKey (MouseButton LeftButton) Down _ pos ->
    (if magV (pos - point0) < knobRadius then state{ selection = Just 0 }
    else if magV (pos - point2) < knobRadius then state{ selection = Just 2 }
    else if magV (pos - point1) < knobRadius then state{ selection = Just 1 }
    else state{ selection = Just 3 }
    ){ cursor = pos }
  EventKey (MouseButton LeftButton) Up _ pos ->
    state{ selection = Nothing, cursor = pos }
  EventMotion pos ->
    (case selection of
      Nothing -> state
      Just 0  -> state{ point0 = point0 + pos - cursor }
      Just 1  -> state{ point1 = point1 + pos - cursor }
      Just 2  -> state{ point2 = point2 + pos - cursor }
      Just 3  -> state{ weight = weight - 0.001 * dotV (pos - cursor) (normaliseV $ rotV $ point2 - point0) }
      ){cursor = pos}
  _ -> state

step :: Float -> State -> State
step _ = id
