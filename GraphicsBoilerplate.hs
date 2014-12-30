module GraphicsBoilerplate where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Control.Monad (when, forM_)
import qualified Graphics as G
import qualified Space as S

graphicsLoop :: Int -> (a -> IO a) -> a -> IO ()
graphicsLoop fps step state = do
  initialize
  loop state
  shutdown
  where
    initialize = do
        True <- GLFW.initialize
        True <- GLFW.openWindow (Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
        GLFW.windowTitle $= "Toasters"
        shadeModel $= Smooth
        lineSmooth $= Enabled
        blend      $= Enabled
        blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
        lineWidth  $= 2.0
        clearColor $= (c 1 1 1)
        GLFW.windowSizeCallback $= \ size@(Size w h) -> do
          viewport   $= (Position 0 0, size)
          matrixMode $= Projection
          loadIdentity
          ortho2D 0 1 0 1
          matrixMode $= Modelview 0
          loadIdentity

    loop state = do
      clear [ColorBuffer]
      loadIdentity
      state' <- step state
      GLFW.swapBuffers
      windowOpen <- GLFW.getParam GLFW.Opened
      escPressed <- keyPressed GLFW.ESC
      let continue = windowOpen && not escPressed
      GLFW.sleep $ 1 / fromIntegral fps
      when continue $ loop state'

    keyPressed :: Enum a => a -> IO Bool
    keyPressed key = fmap (== GLFW.Press) $ GLFW.getKey key

    shutdown = do
      GLFW.closeWindow
      GLFW.terminate

    c :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
    c r g b = Color4 r g b 1

    m :: GLfloat -> Color4 GLfloat
    m v = Color4 v v v 1

    v :: GLfloat -> GLfloat -> Vertex2 GLfloat
    v x y = Vertex2 x y

    vr :: GLfloat -> GLfloat -> Vector3 GLfloat
    vr x y = Vector3 x y 0
