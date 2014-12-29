module Graphics where

import Prelude hiding (lines)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Space as S

import Control.Monad (forM_)

grid :: Int -> Int -> IO () -> IO ()
grid w h action = GL.preservingMatrix $ do
  let w' = f w
      h' = f h
  scale (1 / w') (1 / h')
  setColor gray
  forM_ [0, 1..h] $ \y -> lines $ do
    let y' = f y
    vertex 0  y'
    vertex w' y'
  forM_ [0, 1..w] $ \x -> lines $ do
    let x' = f x
    vertex x' 0
    vertex x' h'
  translate 0.5 0.5
  action

robot color = setColor color >> circle >> line
  where
    circle = lineLoop $
      forM_ [30, 90..330] $ \i ->
        let a = (i * pi / 180)
        in vertex (0.3 * cos a) (0.3 * sin a)

    line = lines $ do
      vertex 0 0
      vertex 0.4 0

pose :: S.Pose -> IO () -> IO ()
pose (S.Pose (S.P x y) (S.D a)) action =
  GL.preservingMatrix $ do
    translate (f x) (f y)
    rotate (f (90 * a))
    action

lineLoop :: IO () -> IO ()
lineLoop = GL.renderPrimitive GL.LineLoop

lines :: IO () -> IO ()
lines = GL.renderPrimitive GL.Lines

setColor :: Color -> IO ()
setColor (C r g b) = GL.color $ GL.Color4 r g b 1

vertex :: F -> F -> IO ()
vertex x y = GL.vertex $ GL.Vertex2 x y

translate :: F -> F -> IO ()
translate x y = GL.translate $ GL.Vector3 x y 0

rotate :: F -> IO ()
rotate a = GL.rotate a (GL.Vector3 0 0 1)

scale :: F -> F -> IO ()
scale x y = GL.scale x y 1

f = fromIntegral

type F = GL.GLfloat

data Color = C F F F

black = C 0 0 0
gray  = C 0.9 0.9 0.9
white = C 1 1 1
red   = C 1 0 0
green = C 0 1 0
blue  = C 0 0 1
