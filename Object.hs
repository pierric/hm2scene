{-# LANGUAGE RecursiveDo, TypeFamilies #-}
module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import Data.IORef
import Data.Fixed
import FRP.Elerea
import Control.Applicative
import Control.Monad
import Control.Monad.State(runStateT)
import Text.Printf
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra as L
import Data.Maybe
import Data.VectorSpace

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

data Once = Once | Continue deriving (Eq,Show)
data Dir  = Wstraight | Wleft | Wright deriving Show 
data Run  = Wwalk | Wrun deriving Show

data RoleState = Idle      Int Int
               | Walk      Int Dir Run Once
               | TurnLeft  Int Once
               | TurnRight Int Once
                 deriving Show 

data Role      = Role{ _rstate   :: RoleState
                     , _rpending :: Maybe RoleState
                     , _rpos   :: Vector3 GLdouble
                     , _rdir   :: GLdouble
                     } deriving Show 

data Camera = Camera{ _csrc :: Vertex3 GLdouble
                    , _cdst :: Vertex3 GLdouble
                    , _cup  :: Vector3 GLdouble 
                    } deriving Show

data SnowP = SnowP{ _ppos  :: Vector3 GLdouble
                  , _pvelocity :: GLdouble }

init_width  = 800
init_height = 600

db_prefix :: [String]
db_prefix     = ["..","tmp"]

main = do 
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithAlphaComponent, WithDepthBuffer]
  initialWindowSize  $= Size init_width init_height
  createWindow "scene"
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
  perspective 45.0 (fromIntegral init_width / fromIntegral init_height) 0.1 100.0
  matrixMode $= Modelview 0

  globalTime <- newIORef 0
  (windowSize,windowSizeSink)       <- external (init_width, init_height)
  (mousePosition,mousePositionSink) <- external (0,0)
  (keyboard, keyboardSink)          <- external Nothing
  (viewMat, viewMatSink)            <- external Mat.identity4
  (projMat, projMatSink)            <- external Mat.identity4


  world  <- newIORef (WorldState{ _filesystem = MockMPQ db_prefix
                                , _resLibrary = (ResourceLibrary glResourceLoader Map.empty)
                                , _db_creaturemodel = Nothing 
                                , _db_creatureskin  = Nothing })

  role <- withWorld world $ newCreature "Creature\\Cat\\Cat" 5

  game <- createSignal $ do
            let scene = pure [] -- scene is a collection of (Signal Objects)
            player <- transfer initPlayer evolvePly (liftA2 (,) keyboard scene)
            let camera = thirdPerson <$> player
            snow   <- transfer initSnow evolveSnow (pure ())
            -- let camera = pure $ Camera (Vertex3 0 10 10) (Vertex3 0 0 0) (Vector3 0 1 0)
            return (renderRole keyboardSink world role <$> player <*> viewMat <*> camera <*> windowSize)

  displayCallback $= return ()
  idleCallback    $= Just (prepare globalTime viewMatSink projMatSink >>= join . superstep game)
  reshapeCallback $= Just (reshape windowSizeSink)
  keyboardMouseCallback $= Just (keypress keyboardSink)
  passiveMotionCallback $= Just (mouse mousePositionSink)
  mainLoop
  
reshape sink (Size w 0) = reshape sink (Size w 1)
reshape sink (Size w h) = do
  viewport   $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (fromIntegral w / fromIntegral h) 0.01 100.0
  matrixMode $= Modelview 0
  sink (w,h)

keypress kSink key Down modifier _ = if key == Char '\27' then leaveMainLoop else kSink $ Just (Down, key, modifier) 
keypress kSink key Up   modifier _ = kSink $ Just (Up, key, modifier)

mouse mSink (Position x y) = do
  mSink (x,y)

prepare time viewSink projSink = do 
  p <- get (matrix (Just Projection)) >>= (getMatrixComponents RowMajor :: GLmatrix GLdouble -> IO [GLdouble])
  projSink $ Mat.fromList $ map realToFrac p
  v <- get (matrix $ Just $ Modelview 0) >>= (getMatrixComponents RowMajor :: GLmatrix GLdouble -> IO [GLdouble])
  viewSink $ Mat.fromList $ map realToFrac v
  o <- get time
  n <- get elapsedTime
  time $= n
  return $ fromIntegral (n-o)

renderRole kSink world dat r@(Role s1 s2 pos dir) view camera wsize@(w,h) = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt (_csrc camera) (_cdst camera) (_cup camera)

  -- render ground
  preservingMatrix $ do --rotate (90 :: GLfloat) (Vector3 (-1) 0 0)
                        scale 6 6 (6 :: GLfloat)
                        renderPrimitive Quads $ do color  $ Color3  1 0 (0 :: GLfloat)
                                                   vertex $ Vertex3 (-1) (0 :: GLfloat) (-1)
                                                   color  $ Color3  0 1 (0 :: GLfloat)
                                                   vertex $ Vertex3 (-1) (0 :: GLfloat) 1
                                                   color  $ Color3  0 0 (1 :: GLfloat)
                                                   vertex $ Vertex3 1    (0 :: GLfloat) 1
                                                   color  $ Color3  1 1 (1 :: GLfloat)
                                                   vertex $ Vertex3 1    (0 :: GLfloat) (-1)

  preservingMatrix $ do translate $ pos
                        rotate (-dir) (Vector3 0 1 0)
                        rotate (90 :: GLfloat) (Vector3 (-1) 0 0)
                        withWorld world $ do
                          Just (GLModel mdl msh) <- findResource (_crea_resource dat)
                          let (anim,time) = case s1 of 
                                              Idle c t         -> (fst (idle !! t), c)
                                              Walk c _ Wwalk _ -> (fst walk, c)
                                              Walk c _ Wrun _  -> (fst run, c)
                                              TurnLeft c _     -> (fst tleft, c)
                                              TurnRight c _    -> (fst tright, c)
                              matrix = transform view anim time (m_bones_ mdl)
                          skeletonAnim matrix msh >>= renderAll (_crea_skin dat)

  windowPos $ Vertex2 10 (fromIntegral (h - 20) :: GLfloat)
  renderString Fixed8By13 (show r)
  windowPos $ Vertex2 10 (fromIntegral (h - 40) :: GLfloat)
  renderString Fixed8By13 (show camera)

  swapBuffers
  kSink Nothing

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
                                     cs  = pos ^+^ Vector3 0 4 0 ^+^ (-5) *^ v
                                     cd  = pos ^+^ 3 *^ v
                                 in  Camera (vert cs) (vert cd) (Vector3 0 1 0)

withWorld w0 action = do
  a     <- readIORef w0
  (b,c) <- runStateT action a
  writeIORef w0 c
  return b

walk = (1, 800)
run  = (2, 667)
idle = [(0,2667),(8,4000),(9,3000)]
sit  = (10, 2000)
stant2sit = (11, 1000)
sit2stand = (12, 1000)
lying     = (13, 2000)
stand2lying = (14, 1333)
lying2stand = (15, 1334)
tleft  = (1,800)
tright = (1,800)

d2r d = d * pi / 180
vert (Vector3 x y z) = Vertex3 x y z
vec  (Vertex3 x y z) = Vector3 x y z

instance AdditiveGroup GLdouble  where
    zeroV   = 0
    (^+^)   = (+)
    negateV = negate

instance VectorSpace GLdouble where
    type Scalar GLdouble = GLdouble
    (*^) = (*)

renderSnowP :: Vector3 Gldouble -> Matrix -> IO ()
renderSnowP p@(Vector3 x y z) view = do
  let up    = 0.5*^normalized (view L.@@> (1,0), view L.@@> (1,1), view L.@@> (1,2))
      right = 0.5*^normalized (view L.@@> (1,0), view L.@@> (1,1), view L.@@> (1,2))
      v0    = p^+^(-1)*^right
      v1    = p^+^up
      v2    = p^+^(-1)*^up
      v3    = p^+^right
  renderPrimitive TriangleStrip $ do vertex . vert $ v0
                                     vertex . vert $ v1
                                     vertex . vert $ v2
                                     vertex . vert $ v3
  