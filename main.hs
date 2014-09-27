{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Data.IORef
import Data.Fixed
import FRP.Elerea.Param
import Control.Applicative
import Control.Monad
import qualified Control.Monad.Reader as R
import Control.Monad.Trans
import Text.Printf
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra as L
import Data.VectorSpace
import Data.Traversable
import System.Random.Mersenne
import Data.Time.Clock
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.OpenGL

-- import Graphics.UI.GLUT(getArgsAndInitialize, renderString, BitmapFont(..))

import Data.WOW.FileSystem
import qualified Data.WOW.Matrix as Mat
import Data.WOW.World
import Data.WOW.M2Model
import Data.WOW.Bone
import Data.WOW.Creature
import Data.WOW.GL.ResourceLoader
import Data.WOW.GL.Types
import Data.WOW.GL.Mesh
import Data.WOW.Utils

import Player
import Terrain
import Snow
import Skybox
import Utils

data Camera = Camera{ _csrc :: Vertex3 GLdouble
                    , _cdst :: Vertex3 GLdouble
                    , _cup  :: Vector3 GLdouble 
                    } deriving Show

canvas_width, canvas_height :: Int
canvas_width  = 800
canvas_height = 600

db_prefix :: [String]
db_prefix     = ["res","mpq"]

type MyWorldState = IORef (WorldState World MockMPQ GLResource)

newtype World a = World { unWorld :: R.ReaderT MyWorldState IO a }

instance M2World World MockMPQ GLResource where
    getWorld   = World $ R.ask >>= (liftIO . readIORef)
    modWorld f = World $ R.ask >>= (liftIO . flip modifyIORef' f)
instance Monad World where
    return = World . return
    a >>= f = World (unWorld a >>= (\v -> unWorld (f v)))
instance MonadIO World where
    liftIO = World . liftIO

runInWorld :: MyWorldState -> World a -> IO a
runInWorld w a = R.runReaderT (unWorld a) w

main = do
  -- getArgsAndInitialize 
  initGUI
  initGL

  -- create window
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8
             , windowTitle := "Scene" ]

  config <- glConfigNew [GLModeRGBA, GLModeAlpha, GLModeDepth, GLModeDouble]
  canvas <- glDrawingAreaNew config
  widgetSetSizeRequest canvas canvas_width canvas_height

  set window [ containerChild := canvas ]

  globalTime <- getCurrentTime >>= newIORef
  (windowSize,windowSizeSink)       <- external (canvas_width, canvas_height)
  (mousePosition,mousePositionSink) <- external (0,0)
  (keyboard, keyboardSink)          <- external Nothing
  (viewMat, viewMatSink)            <- external Mat.identity4
  (projMat, projMatSink)            <- external Mat.identity4
  (random, randomSink)              <- external ([] :: [Int])

  myGame   <- newIORef undefined
  realized <- newIORef False
  -- setup events
  onRealize canvas (withGLDrawingArea canvas $ \_ -> 
                        myInit myGame windowSize mousePosition keyboard viewMat projMat random >> writeIORef realized True
                   )
  onExpose canvas (\_ -> 
    do withGLDrawingArea canvas (\w -> 
         do game <- readIORef myGame
            dt <- prepare globalTime randomSink viewMatSink projMatSink
            join $ game dt
            glDrawableSwapBuffers w
            keyboardSink Nothing
         )
       return True)
  timeoutAddFull (widgetQueueDraw canvas >> return True) priorityDefaultIdle 10
  onKeyPress     window (keypress keyboardSink)
  onKeyRelease   window (keypress keyboardSink)
  onMotionNotify window True (mouse mousePositionSink)
  onConfigure canvas (reshape realized windowSizeSink)
  widgetShowAll window
  mainGUI

myInit gameLogic  windowSize mousePosition keyboard viewMat projMat random = do
  clearColor $= Color4 0 0 0 1
  clearDepth $= 1
  depthFunc  $= Just Less
  blend      $= Disabled
  shadeModel $= Smooth
  glEnable gl_TEXTURE_2D
  lighting   $= Enabled
  ambient  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  diffuse  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  position (Light 1) $= Vertex4 0 0 5 (1.0 :: GLfloat)
  light    (Light 1) $= Enabled
  hint PerspectiveCorrection $= Nicest
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (fromIntegral canvas_width / fromIntegral canvas_height) 0.1 100.0
  matrixMode $= Modelview 0


  world  <- newIORef (WorldState{ _filesystem = MockMPQ db_prefix
                                , _resLibrary = (ResourceLibrary glResourceLoader Map.empty)
                                , _db_creaturemodel = Nothing 
                                , _db_creatureskin  = Nothing })

  role <- runInWorld world $ newCreature "Creature\\Cat\\Cat" (5 :: Int)

  (ig,eg,rg) <- terrain
  (is,es,rs) <- snow
  (ik,ek,rk) <- skybox

  g <- start $ do let scene = pure [] -- scene is a collection of (Signal Objects)
                  player <- transfer initPlayer evolvePly (liftA2 (,) keyboard scene)
                  let camera = thirdPerson <$> player
                  -- let camera = pure $ Camera (Vertex3 0 10 10) (Vertex3 0 0 0) (Vector3 0 1 0)
                  ground   <- transfer ig eg (pure ())
                  snow     <- transfer is es random
                  skybox   <- transfer ik ek ((vec . _csrc) <$> camera)
                  let renderGround = pure rg
                      renderSnow   = rs <$> snow
                      renderSky    = rk <$> skybox
                  return (renderScene world role <$> viewMat <*> camera <*> 
                                                     windowSize <*> keyboard  <*> mousePosition<*> 
                                                     player <*> renderGround <*> renderSnow <*> renderSky)
  writeIORef gameLogic g

prepare time randomSink viewSink projSink = do 
  p <- get (matrix (Just Projection)) >>= (getMatrixComponents RowMajor :: GLmatrix GLdouble -> IO [GLdouble])
  projSink $ Mat.fromList p
  v <- get (matrix $ Just $ Modelview 0) >>= (getMatrixComponents RowMajor :: GLmatrix GLdouble -> IO [GLdouble])
  viewSink $ Mat.fromList v
  
  gen <- getStdGen
  randoms gen >>= randomSink

  o <- get time
  n <- getCurrentTime
  time $= n
  return $ (1000 * diffUTCTime n o)

reshape realized sink (Configure _ _ _ w h) = do
  realized <- readIORef realized
  if not realized 
    then return True
    else do putStrLn $ show w ++ " " ++ show h
            viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
            matrixMode $= Projection
            loadIdentity
            perspective 45.0 (fromIntegral w / fromIntegral h) 0.01 100.0
            matrixMode $= Modelview 0
            sink (w,h)
            return True

keypress kSink (Key rel _ _ mods _ _ _ val name _) = kSink (Just (rel, name, mods)) >> return True
mouse mSink (Motion _ _ x y _ _ _ _) = mSink (x,y) >> return True

renderScene world dat view camera wsize@(w,h) key mouse r@(Role s1 s2 pos dir) renderGround renderSnow renderSky = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt (_csrc camera) (_cdst camera) (_cup camera)

  renderSky
  renderGround

  preservingMatrix $ do translate $ pos
                        rotate (-dir) (Vector3 0 1 0)
                        rotate (90 :: GLfloat) (Vector3 (-1) 0 0)
                        runInWorld world $ do
                          Just (GLModel mdl msh) <- findResource (_crea_resource dat)
                          let (anim,time) = case s1 of 
                                              Idle c t         -> (fst (idle !! t), c)
                                              Walk c _ Wwalk _ -> (fst walk, c)
                                              Walk c _ Wrun _  -> (fst run, c)
                                              TurnLeft c _     -> (fst tleft, c)
                                              TurnRight c _    -> (fst tright, c)
                              matrix = transform view anim time (m_bones_ mdl)
                          skeletonAnim matrix msh >>= renderAll (_crea_skin dat)

  renderSnow view
  {--
  windowPos $ Vertex2 10 (fromIntegral (h - 20) :: GLfloat)
  renderString Fixed8By13 (show r)
  windowPos $ Vertex2 10 (fromIntegral (h - 40) :: GLfloat)
  renderString Fixed8By13 (show camera)
  --}

-- Caution : This implementation seems wrong.
direction :: (GLint,GLint) -> (GLsizei,GLsizei) -> Mat.Matrix -> Mat.Matrix -> Vector3 Float
direction (px,py) (sw,sh) view proj =
    let fx = fromIntegral px
        fy = fromIntegral py 
        fw = fromIntegral sw
        fh = fromIntegral sh 
        (proj_px,proj_py) = (2*fx/fw-1, 2*fy/fh-1)
        (view_px,view_py) = (proj_px / (realToFrac $ proj L.@@> (0,0)), proj_py / (realToFrac $ proj L.@@> (1,1)))
        iv                = Mat.inverse view
        Vector3 dx dy dz  = Mat.multVec3 iv (Vector3 view_px view_py 1)
        (ox,oy,oz)        = (realToFrac $ iv L.@@> (0,3), realToFrac $ iv L.@@> (1,3), realToFrac $ iv L.@@> (2,3))
        (x,y,z)           = ((dx-ox), (dy-oy), (dz-oz))
    in  Vector3 (-oy * x / y + ox) 0 (-oy * z / y + oz)

thirdPerson (Role _ _ pos dir) = let v   = Vector3 (cos $ d2r dir) 0 (sin $ d2r dir)
                                     cs  = pos ^+^ Vector3 0 2.5 0 ^+^ (-5) *^ v
                                     cd  = pos ^+^ Vector3 0 1   0 ^+^ 3 *^ v
                                 in  Camera (vert cs) (vert cd) (Vector3 0 1 0)
