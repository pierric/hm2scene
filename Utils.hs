{-# LANGUAGE TypeFamilies #-}
module Utils where

import Graphics.UI.GLUT
import Data.VectorSpace
import Data.IORef
import Control.Monad.State

withWorld w0 action = do
  a     <- readIORef w0
  (b,c) <- runStateT action a
  writeIORef w0 c
  return b

d2r d = d * pi / 180
vert (Vector3 x y z) = Vertex3 x y z
vec  (Vertex3 x y z) = Vector3 x y z

{--
instance AdditiveGroup GLdouble  where
    zeroV   = 0
    (^+^)   = (+)
    negateV = negate

instance VectorSpace GLdouble where
    type Scalar GLdouble = GLdouble
    (*^) = (*)

instance InnerSpace GLdouble where
    (<.>)   = (*)
--}
