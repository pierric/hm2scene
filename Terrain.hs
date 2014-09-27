module Terrain where

import Graphics.UI.GLUT
import Codec.Image.STB

data Terrain = Terrain

terrain = do
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  textureFilter  Texture2D $= ((Linear', Nothing), Linear')
  Right image <- loadImage "res/wall.jpg"
  withImage image (\ptr (w,h) chn -> build2DMipmaps Texture2D RGB' w h (PixelData RGB UnsignedByte ptr))
  return (Terrain, \ _ _ -> id, renderTerrain tex)

renderTerrain tex = do 
  textureBinding Texture2D $= Just tex
  preservingMatrix $ do scale 30 30 (30 :: GLfloat)
                        let r = 10
                        renderPrimitive Quads $ do texCoord (TexCoord2 0 (r ::GLfloat))
                                                   normal $ Normal3 0 1 (0 :: GLfloat)
                                                   vertex $ Vertex3 (-1) (0 :: GLfloat) (-1)
                                                   texCoord (TexCoord2 r (r ::GLfloat))
                                                   normal $ Normal3 0 1 (0 :: GLfloat)
                                                   vertex $ Vertex3 (-1) (0 :: GLfloat) 1
                                                   texCoord (TexCoord2 r (0 ::GLfloat))
                                                   normal $ Normal3 0 1 (0 :: GLfloat)
                                                   vertex $ Vertex3 1    (0 :: GLfloat) 1
                                                   texCoord (TexCoord2 0 (0 ::GLfloat))
                                                   normal $ Normal3 0 1 (0 :: GLfloat)
                                                   vertex $ Vertex3 1    (0 :: GLfloat) (-1)
