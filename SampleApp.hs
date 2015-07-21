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
import Graphics.UI.GLFW

import System.FilePath (splitFileName, (</>))

import Control.Monad (forM_, liftM)
import Data.Maybe    (catMaybes)
import Data.Either   (rights, lefts)
import qualified Data.Map as Map

import Text.Printf (printf)

import Data.IORef

import qualified Southpaw.WaveFront.Parsers as WF --  (loadModel, loadOBJ, loadMTL, facesOf, Model(..), Face(..), OBJToken(..), Material(..))
import Southpaw.Utilities.Utilities (numeral)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- | 
type Buffers  = [(Normal3 GLfloat, WF.Material, [Vertex3 GLfloat])]
data AppState = AppState { _rotation :: (GLint, GLint), _mouse :: Maybe (GLint, GLint), _clientsize :: (GLint, GLint) } deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | 
initOpenGL :: IO ()
initOpenGL = do
	diffuse  (Light 0) $= Color4  1.0 1.0 1.0 1.0
	position (Light 0) $= Vertex4 1.0 1.0 4.5 0.0

	-- light   (Light 0) $= Enabled -- TODO: Vertex colours seem to be ignored when lighting is enabled
	-- lighting          $= Enabled --

	depthFunc $= Just Lequal

	matrixMode  $= Projection
	perspective 40.0 1.0 1.0 10.0

	matrixMode $= Modelview 0
	lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

	translate    ((Vector3 0.0 0.0 (-4.0)) :: Vector3 GLfloat)
	rotate (-30) ((Vector3 1.0 0.0   0.0)  :: Vector3 GLfloat)
	rotate (-20) ((Vector3 0.0 0.0   1.0)  :: Vector3 GLfloat)

	-- scale (0.05 :: GLfloat) 0.5 0.5


-- | 
--
-- TODO: Simplify, refactor, better names
--
createBuffers :: WF.Model -> Buffers
createBuffers model = triplets normals' (map WF.material faces') vertices'
	where faces'     = WF.faces model
	      vertexdata = WF.vertices model
	      normaldata = WF.normals model

	      normals'  = map normalOf faces'
	      vertices' = map verticesOf faces'

	      normalOf   face = normalAt . head . catMaybes . map third $ WF.indices face -- TODO: Don't use catMaybes
	      verticesOf face = map (vertexAt . first) $ WF.indices face
	      
	      normalAt i = triplet Normal3 $ normaldata !! (i-1) -- TODO: Make sure the subtraction isn't performed by the parsers
	      vertexAt i = triplet Vertex3 $ vertexdata !! (i-1)

	      triplet f (x, y, z) = f (realToFrac x) (realToFrac y) (realToFrac z)
	      first (v, _, _)     = v
	      third (_, _, n)     = n

	      triplets = zip3


-- |
onmousedrag :: IORef AppState -> MotionCallback
onmousedrag stateref (Position mx my) = modifyIORef stateref $ \ state -> case state of
	AppState { _mouse=Nothing }                             -> state { _mouse=Just (mx, my) }
	AppState { _rotation=(rx, ry), _mouse=Just (mx', my') } -> state { _rotation=(rx+mx-mx', ry+my-my'), _mouse=Just (mx, my) }


-- 
-- TODO: Rename (?)
onmouseup :: IORef AppState -> MouseCallback
onmouseup stateref _ Up _ = modifyIORef stateref $ \ state -> state { _mouse=Nothing }
onmouseup _        _ _  _ = return ()


-- |
onwindowresize :: IORef AppState -> ReshapeCallback
onwindowresize stateref (Size cx cy) = do
	modifyIORef stateref $ \ state -> state { _clientsize=(cx, cy) }
	viewport $= (Position 0 0, Size cx cy)


-- |
render :: IORef AppState -> [Buffers] -> DisplayCallback
render stateref buffers = do
	--
	(rxi, ryi) <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref

	forM_ [(0, 0)] $ \ (wxi, wyi) -> do
		--
		viewport   $= (Position (wxi*div cxi 2) (wyi*div cyi 2), Size (div cxi 2) (div cyi 2))
		clearColor $= Color4 (0.5*float wxi) (0.5*float wyi) (float wxi*float wyi*0.3) (1.0 :: GLfloat)
		scissor    $= Just (Position (wxi*div cxi 2) (wyi*div cyi 2), Size (div cxi 2) (div cyi 2))

		-- TODO: Refactor with preservingMatrix (?)
		matrixMode $= Modelview 0
		loadIdentity
		lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

		translate    ((Vector3 0.0 (-2.0) (-4.0)) :: Vector3 GLfloat)
		rotate (180 * float rxi/float cxi) ((Vector3 0.0 1.0 0.0) :: Vector3 GLfloat)
		rotate (180 * float ryi/float cyi) ((Vector3 1.0 0.0 0.0) :: Vector3 GLfloat)

		clear [ColorBuffer, DepthBuffer]
		forM_ buffers renderModel
		swapBuffers
	where float = realToFrac


-- |
-- TODO: Colours and textures
-- TODO: Arbitrary polygons
-- TODO: Refactor, simplify
renderModel :: Buffers -> IO ()
renderModel buffers = forM_ buffers renderFace


-- | (Normal)
--
-- TODO: Why just triangles (?)
--
renderFace :: (Normal3 GLfloat, WF.Material, [Vertex3 GLfloat]) -> IO ()
renderFace (n, mat, vertices) =  renderPrimitive TriangleFan $ do color3f mat
                                                                  normal n
                                                                  mapM_ vertex vertices
    where color3f mat = let (r,g,b,_) = WF.diffuse mat in (color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GLfloat))


-- |
showVertex (Vertex3 x y z) = show $ (show x, show y, show z)


-- |
showNormal (Normal3 x y z) = show $ (show x, show y, show z)


-- | 
animate fps = do
	postRedisplay Nothing
	addTimerCallback (div 1000 fps) (animate fps)



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
	--
	let path      = "C:/Users/Jonatan/Desktop/3D/models/"
	let modelname = "king.obj"
	putStrLn "Loading model..."
	model <- WF.loadModel $ path </> modelname
	printf "Finished loading model '%s' with %d faces and %d vertices.\n" modelname (length $ WF.faces model) (length $ WF.vertices model)

	--
	stateref <- newIORef AppState { _rotation=(0,0), _mouse=Nothing, _clientsize=(720, 480) } --
	(cx, cy) <- liftM _clientsize . readIORef $ stateref                                      -- Yes, I know this is stupid.

	monitor <- getPrimaryMonitor
	window  <- createWindow cx cy "WaveFront OBJ Sample (2015)" mmon mwin

	initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling]
	initialWindowSize  $= (Size cx cy)

	putStrLn "Creating buffers..."
	let buffers = [createBuffers model]


	displayCallback $= (render stateref buffers)
	motionCallback  $= Just (onmousedrag stateref)
	mouseCallback   $= Just (onmouseup stateref)
	reshapeCallback $= Just (onwindowresize stateref)
	addTimerCallback (div 100 30) (animate 30)

	putStrLn "Finished creating buffers."
	initOpenGL
	mainLoop

	putStrLn "Finished"
	mapM_ putStrLn ["Finished painting.",
	                "Washing brushes...",
	                "Mounting canvas...",
	                "Disassembling easel...",
	                "Done. Good bye!"]