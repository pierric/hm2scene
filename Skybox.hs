module Skybox where

import Graphics.UI.GLUT
import Codec.Image.STB

data Skybox = Skybox{ _center :: Vector3 GLdouble }

skybox = do
  textures@[ft,lf,bk,rt,up,dn] <- genObjectNames 6
  let files = ["res/irrlicht2_ft.jpg"
              ,"res/irrlicht2_lf.jpg"
              ,"res/irrlicht2_bk.jpg"
              ,"res/irrlicht2_rt.jpg"
              ,"res/irrlicht2_up.jpg"
              ,"res/irrlicht2_dn.jpg"]
  mapM_ (\(tex,fn) -> do Right img <- loadImage fn
                         textureBinding Texture2D $= Just tex
                         textureFilter  Texture2D $= ((Linear', Nothing), Linear')
                         textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
                         textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
                         withImage img (\ptr (w,h) chn -> build2DMipmaps Texture2D RGB' w h (PixelData RGB UnsignedByte ptr))
        ) $ zip textures files  
  return (Skybox (Vector3 0 0 0), evolveSkybox, renderSkybox ft lf bk rt up dn)

evolveSkybox _ center _ = Skybox center

renderSkybox ft lf bk rt up dn (Skybox center)= do
  df <- get depthFunc
  depthFunc $= Nothing
  depthMask $= Disabled
  lighting  $= Disabled
  preservingMatrix $ do 
    translate $ center
    -- front
    textureBinding Texture2D $= Just ft
    renderPrimitive Quads $ do mkVert (-1,-1,-1) (0,0,1) (1,1)
                               mkVert (1,-1,-1)  (0,0,1) (0,1)
	                       mkVert (1, 1,-1)  (0,0,1) (0,0)
	                       mkVert (-1, 1,-1) (0,0,1) (1,0)
    -- left
    textureBinding Texture2D $= Just lf
    renderPrimitive Quads $ do mkVert (1,-1,-1) (-1,0,0) (1,1)
  	                       mkVert (1,-1, 1) (-1,0,0) (0,1)
	                       mkVert (1, 1, 1) (-1,0,0) (0,0)
	                       mkVert (1, 1,-1) (-1,0,0) (1,0)
    -- back
    textureBinding Texture2D $= Just bk
    renderPrimitive Quads $ do mkVert (1,-1, 1)  (0,0,-1) (1,1)
	                       mkVert (-1,-1, 1) (0,0,-1) (0,1)
                               mkVert (-1, 1, 1) (0,0,-1) (0,0)
                               mkVert ( 1, 1, 1) (0,0,-1) (1,0)
    -- right
    textureBinding Texture2D $= Just rt
    renderPrimitive Quads $ do mkVert (-1,-1, 1) (1,0,0) (1,1)
	                       mkVert (-1,-1,-1) (1,0,0) (0,1)
	                       mkVert (-1, 1,-1) (1,0,0) (0,0)
	                       mkVert (-1, 1, 1) (1,0,0) (1,0)
    -- top
    textureBinding Texture2D $= Just up
    renderPrimitive Quads $ do mkVert (1, 1,-1)  (0,-1,0) (1,1)
	                       mkVert (1, 1, 1)  (0,-1,0) (0,1)
	                       mkVert (-1, 1, 1) (0,-1,0) (0,0)
	                       mkVert (-1, 1,-1) (0,-1,0) (1,0)
    -- bottom
    textureBinding Texture2D $= Just dn
    renderPrimitive Quads $ do mkVert (1,-1, 1)  (0,1,0) (0,0)
	                       mkVert (1,-1,-1)  (0,1,0) (1,0)
	                       mkVert (-1,-1,-1) (0,1,0) (1,1)
	                       mkVert (-1,-1, 1) (0,1,0) (0,1)
  depthMask $= Enabled
  depthFunc $= df
  lighting  $= Enabled

    where mkVert :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
          mkVert (vx,vy,vz) (nx,ny,nz) (tx,ty) = do normal   $ Normal3   nx ny nz
                                                    texCoord $ TexCoord2 tx ty
                                                    vertex   $ Vertex3   vx vy vz
