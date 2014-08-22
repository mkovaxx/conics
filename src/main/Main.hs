{-# LANGUAGE RecordWildCards #-}
import Data.List (sort)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Raster.Field

import Conic
import Util

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
  , selection :: Maybe Int
  , knobRadius :: Int
  , cursor :: Point
  , point0 :: Point
  , point1 :: Point
  , point2 :: Point
  , weight :: Float
  , raySrc :: Point
  , rayDst :: Point
  }

initialize :: State
initialize =
  State
  { size = (800, 600)
  , selection = Nothing
  , knobRadius = 31
  , cursor = (0, 0)
  , point0 = (40, 250)
  , point1 = (0, 0)
  , point2 = (200, 60)
  , weight = 1
  , raySrc = (-250, -40)
  , rayDst = (-60, -200)
  }

draw :: State -> Picture
draw State{..} =
  Pictures [plot, secant, hud]
 where
  hud = Color black $ Pictures
    [ Line [point0, point1]
    , Line [point1, point2]
    , Line [ mulSV (1 / (1 + absW)) (mulSV absW point1 + point0)
           , mulSV (1 / (1 + absW)) (mulSV absW point1 + point2)]
    , uncurry Translate point0 $ Circle $ fromIntegral knobRadius
    , uncurry Translate point1 $ Circle $ fromIntegral knobRadius
    , uncurry Translate point2 $ Circle $ fromIntegral knobRadius
    , uncurry Translate raySrc $ Circle $ fromIntegral knobRadius
    , uncurry Translate rayDst $ Circle $ fromIntegral knobRadius
    ]
  absW = abs weight
  bezier =
    Bezier
    { bezierP0 = point0
    , bezierP1 = point1
    , bezierP2 = point2
    , bezierW  = weight
    }
  rayDir = rayDst - raySrc
  conic = bezierToConic bezier
  plot = plotConicSlice size white violet conic
  secant = plotSecant size red green conic raySrc rayDir

input :: Event -> State -> State
input event state@State{..} = case event of
  EventResize newSize ->
    state{ size = newSize }
  EventKey (MouseButton WheelUp) Down _ pos ->
    state{ knobRadius = knobRadius + 1, cursor = pos }
  EventKey (MouseButton WheelDown) Down _ pos ->
    state{ knobRadius = knobRadius - 1, cursor = pos }
  EventKey (MouseButton LeftButton) Down _ pos ->
    (if magV (pos - point0) < fromIntegral knobRadius then state{ selection = Just 0 }
    else if magV (pos - point2) < fromIntegral knobRadius then state{ selection = Just 2 }
    else if magV (pos - point1) < fromIntegral knobRadius then state{ selection = Just 1 }
    else if magV (pos - raySrc) < fromIntegral knobRadius then state{ selection = Just 4 }
    else if magV (pos - rayDst) < fromIntegral knobRadius then state{ selection = Just 5 }
    else state{ selection = Just (-1) }
    ){ cursor = pos }
  EventKey (MouseButton LeftButton) Up _ pos ->
    state{ selection = Nothing, cursor = pos }
  EventMotion pos ->
    (case selection of
      Nothing   -> state
      Just 0    -> state{ point0 = point0 + pos - cursor }
      Just 1    -> state{ point1 = point1 + pos - cursor }
      Just 2    -> state{ point2 = point2 + pos - cursor }
      Just 3    -> state
      Just 4    -> state{ raySrc = raySrc + pos - cursor }
      Just 5    -> state{ rayDst = rayDst + pos - cursor }
      Just (-1) -> state{ weight = let
                                     median = mulSV 0.5 (point0 + point2) - point1
                                     p = pos - point1
                                     cosa = dotV p median / dotV median median
                                   in 1.0 / cosa - 1.0
                        }
      ){cursor = pos}

  _ -> state

step :: Float -> State -> State
step _ = id

plotConicSlice :: (Int, Int) -> Color -> Color -> Conic -> Picture
plotConicSlice size positive negative conic =
  Pictures [plotSecant size positive negative conic (0.0, fromIntegral y) (1.0, 0.0) | y <- [-snd size .. snd size]]

plotConic :: (Int, Int) -> Color -> Color -> Conic -> Picture
plotConic size positive negative conic =
  makePicture (fst size) (snd size) 1 1
    ( (\v -> if v > 0 then positive else negative)
    . evalConic conic
    . (\(x, y) -> (0.5 * fromIntegral (fst size) * x, 0.5 * fromIntegral (snd size) * y))
    )

plotSecant :: (Int, Int) -> Color -> Color -> Conic -> Vector -> Vector -> Picture
plotSecant size positive negative conic raySrc rayDir =
  Pictures $ zipWith (colorSegment conic positive negative) points (tail points)
 where
  points = [raySrc + mulSV t rayDir | t <- params]
  params = sort $ plotBounds ++ map clamp shapeBounds
  clamp = max plotMin . min plotMax
  shapeBounds = pierceConic conic raySrc rayDir
  plotBounds = pierceBox viewBox raySrc rayDir
  [plotMin, plotMax] = plotBounds
  viewBox = mulSV 0.5 (fromIntegral $ fst size, fromIntegral $ snd size)

colorSegment :: Conic -> Color -> Color -> Point -> Point -> Picture
colorSegment conic positive negative a b =
  Color color $ Line [a, b]
 where
  color = if evalConic conic midPoint > 0 then positive else negative
  midPoint = mulSV 0.5 $ a + b

pierceBox :: (Float, Float) -> Vector -> Vector -> [Float]
pierceBox (width, height) (xs, ys) (xd, yd) =
  if minT < maxT then [minT, maxT] else [0, 0]
 where
  minT = max xt0 yt0
  maxT = min xt1 yt1
  [xt0, xt1] = sort $ [(c - xs) / xd | c <- [-width , width ]]
  [yt0, yt1] = sort $ [(c - ys) / yd | c <- [-height, height]]
