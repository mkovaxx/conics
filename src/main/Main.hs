{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game

knobRadius = 6

main :: IO ()
main = do
  let
    position = (10, 10)
  play
    (InWindow "Nice Window" (size initState) position)
    white
    0
    initState
    drawState
    moveState
    stepState

data State =
  State
  { size :: (Int, Int)
  , cursor :: (Float, Float)
  , point_00 :: (Float, Float)
  , point_11 :: (Float, Float)
  , point_01 :: (Float, Float)
  , weight :: Float
  , selection :: Maybe Int
  }

initState :: State
initState =
  State
  { size = (800, 600)
  , cursor = (0, 0)
  , point_00 = (-40, -40)
  , point_11 = (250, 250)
  , point_01 = (200, -200)
  , weight = 1
  , selection = Nothing
  }

drawState :: State -> Picture
drawState State{..} =
  Color black $ Pictures
    [ Line [point_00, point_01]
    , Line [point_01, point_11]
    , uncurry Translate point_00 $ Circle knobRadius
    , uncurry Translate point_11 $ Circle knobRadius
    , uncurry Translate point_01 $ Circle knobRadius
    ]

moveState :: Event -> State -> State
moveState event state@State{..} = case event of
  EventResize newSize ->
    state{ size = newSize }
  EventKey (MouseButton LeftButton) Down _ pos ->
    (if magV (pos - point_00) < knobRadius then state{ selection = Just 0 }
    else if magV (pos - point_11) < knobRadius then state{ selection = Just 1 }
    else if magV (pos - point_01) < knobRadius then state{ selection = Just 2 }
    else state){ cursor = pos }
  EventKey (MouseButton LeftButton) Up _ pos ->
    state{ selection = Nothing, cursor = pos }
  EventMotion pos ->
    (case selection of
      Nothing -> state
      Just 0  -> state{ point_00 = point_00 + pos - cursor }
      Just 1  -> state{ point_11 = point_11 + pos - cursor }
      Just 2  -> state{ point_01 = point_01 + pos - cursor }
      ){cursor = pos}
  _ -> state

stepState :: Float -> State -> State
stepState _ = id
