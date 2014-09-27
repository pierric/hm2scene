{-# LANGUAGE BangPatterns #-}
module Snow(snow) where

import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.UI.GLUT
import Numeric.LinearAlgebra as L
import Data.VectorSpace
import Text.Printf
import Codec.Image.STB
import System.Random.Mersenne

import Utils
import Data.WOW.Utils
import qualified Data.WOW.Matrix as Mat

maxS  = 2000
rateS = (0,30)

data SnowP = SnowP{ _pos   :: Vector3 GLdouble
                  , _size  :: GLdouble
                  , _speed :: Vector3 GLdouble 
                  } deriving Show           

snow :: Real p => IO ([SnowP], p -> [Int] -> [SnowP] -> [SnowP], [SnowP] -> Mat.Matrix -> IO ())
snow = do rand <- getStdGen >>= randoms
          [tex] <- genObjectNames 1
          textureBinding Texture2D $= Just tex
          textureFilter  Texture2D $= ((Linear', Nothing), Linear')
          Right image <- loadImage "res/Snow.bmp"
          withImage image (\ptr (w,h) chn -> build2DMipmaps Texture2D RGB' w h (PixelData RGB UnsignedByte ptr))
          return (mkSnowP (snd rateS) rand, evolveSnow, renderSnow tex)

mkSnowP :: Int -> [Int] -> [SnowP]
mkSnowP 0 _ = []
mkSnowP (n+1) (rx:rz:rs:rr) = let fx = fromIntegral rx / fromIntegral (maxBound :: Int)
                                  fz = fromIntegral rz / fromIntegral (maxBound :: Int)
                                  fs = fromIntegral rs / fromIntegral (maxBound :: Int)
                                  x = 20 * fx
                                  z = 20 * fz
                                  s = (fs / 10000) - 0.0008
                              in  SnowP (Vector3 x 6 z) 0.05 (Vector3 0 s 0) : mkSnowP n rr

evolveSnow dt (r0:rs) sn = let (!live,!cnt) = update sn [] 0
                               !n = min (mod r0 (1 + snd rateS - fst rateS) + fst rateS) (maxS - cnt)
                               !new = mkSnowP n rs
                           in  live ++ new
    where 
      update [] !c !n = (c,n)
      update (s:sn) !c !n = let p'@(Vector3 _ y _) = _pos s ^+^ (realToFrac dt *^ _speed s)
                            in  if y <= 0
                                then update sn c n
                                else update sn (s{_pos=p'}:c) (n+1)

renderSnow tex snow view = do
  let right = normalized $ Vector3 (convert $ view @@> (0,0)) (convert $ view @@> (0,1)) (convert $ view @@> (0,2))
      up    = normalized $ Vector3 (convert $ view @@> (1,0)) (convert $ view @@> (1,1)) (convert $ view @@> (1,2))
  lighting  $= Disabled
  textureBinding Texture2D $= Just tex
  blend     $= Enabled
  blendFunc $= (One,One)
  preservingMatrix $ mapM (\sn -> let p = _pos sn
                                      w = _size sn *^ right
                                      h = _size sn *^ up
                                  in  renderPrimitive Quads $ do texCoord (TexCoord2 0 (0 ::GLfloat))
                                                                 vertex . vert $ p ^-^ w
                                                                 texCoord (TexCoord2 0 (1 ::GLfloat))
                                                                 vertex . vert $ p ^+^ h
                                                                 texCoord (TexCoord2 1 (1 :: GLfloat))
                                                                 vertex . vert $ p ^+^ w
                                                                 texCoord (TexCoord2 1 (0 :: GLfloat))
                                                                 vertex . vert $ p ^-^ h
                          ) snow
  blend $= Disabled
  textureBinding Texture2D $= Nothing
  lighting   $= Enabled
  {--
  case snow of 
    []  -> return ()
    s:_ -> do windowPos $ Vertex2 10 (fromIntegral 10 :: GLfloat)
              renderString Fixed8By13 (showV $ _pos s)
  --}

showV (Vector3 x y z) = printf "(%.5f,%.5f,%.5f)" (convert' x) (convert' y) (convert' z)
convert = realToFrac :: Double -> GLdouble
convert'= realToFrac :: GLdouble -> Double
