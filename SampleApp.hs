-- |
-- Module      : Southpaw.WaveFront.SampleApp
-- Description : Exectuble which loads a model and displays it using OpenGL
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 15 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.WaveFront.SampleApp where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Monad (forM_)
import Data.Maybe    (catMaybes)

import Southpaw.WaveFront.Parsers (loadModel, Model(..), Face(..))



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Buffers = [(Normal3 GLfloat, [Vertex3 GLfloat])]



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
initOpenGL :: IO ()
initOpenGL = do
	diffuse  (Light 0) $= Color4  0.5 0.7 0.2 1.0
	position (Light 0) $= Vertex4 1.0 1.0 1.5 0.0

	light   (Light 0) $= Enabled
	lighting          $= Enabled

	depthFunc $= Just Lequal

	matrixMode  $= Projection
	perspective 40.0 1.0 1.0 10.0

	matrixMode $= Modelview 0
	lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

	translate    ((Vector3 0.0 0.0 (-2.0)) :: Vector3 GLfloat)
	rotate   50  ((Vector3 1.0 0.0   0.0)  :: Vector3 GLfloat)
	rotate (-20) ((Vector3 0.0 0.0 1.0)    :: Vector3 GLfloat)


-- |
-- TODO: Simplify, refactor, better names
createBuffers :: Model -> Buffers
createBuffers model = zip normals' vertices'
	where faces'     = faces model
	      vertexdata = vertices model
	      normaldata = normals model

	      normals'  = map normalOf faces'
	      vertices' = map verticesOf faces'

	      normalOf   face = normalAt . head . catMaybes . map normal $ indices face
	      verticesOf face = map (vertexAt . vertex) $ indices face
	      
	      normalAt i = triplet Normal3 $ normaldata !! i
	      vertexAt i = triplet Vertex3 $ vertexdata !! i

	      triplet f (x, y, z) = f (realToFrac x) (realToFrac y) (realToFrac z)
	      vertex (v, _, _)    = v
	      normal (_, _, n)    = n



-- |
render :: [Buffers] -> DisplayCallback
render buffers = do
	clear [ColorBuffer, DepthBuffer]
	mapM_ renderModel buffers
	swapBuffers


-- |
-- TODO: Colours and textures
-- TODO: Arbitrary polygons
-- TODO: Refactor, simplify
renderModel :: Buffers -> IO ()
renderModel buffers = forM_ buffers $ \ (n, [va, vb, vc]) -> do
	renderPrimitive Triangles $ do
		normal n
		vertex va
		vertex vb
		vertex vc



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
	--
	model <- loadModel "C:/Users/Jonatan/Desktop/3D/minecraft/minecraft1.obj"

	--
	getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
	createWindow "WaveFront OBJ Sample (2015)"
	displayCallback $= (render [createBuffers model])
	initOpenGL
	mainLoop
	mapM_ putStrLn ["Finished painting.",
	                "Washing brushes...",
	                "Mounting canvas...",
	                "Disassembling easel...",
	                "Done. Good bye!"]