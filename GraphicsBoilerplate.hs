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
        clearColor $= Color4 1 1 1 1
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
