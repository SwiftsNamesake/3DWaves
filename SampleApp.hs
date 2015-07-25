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
import Graphics.Rendering.OpenGL.GL.Shaders
import Graphics.Rendering.OpenGL.GL.ByteString (packUtf8)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW as GLFW (MouseButton(..), KeyState(..), MouseButtonState(..))

import System.FilePath (splitFileName, (</>))

import Control.Monad (forM_, liftM, unless)
import Data.Maybe    (catMaybes)
import Data.Either   (rights, lefts)
import qualified Data.Map as Map

import Text.Printf (printf)

import Data.IORef

import qualified Southpaw.WaveFront.Parsers as WF --  (loadModel, loadOBJ, loadMTL, facesOf, Model(..), Face(..), OBJToken(..), Material(..))
import Southpaw.Utilities.Utilities (numeral, gridM)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- | 
type Buffers  = [(Normal3 GLfloat, WF.Material, [Vertex3 GLfloat])]
data AppState = AppState { _rotation :: (Double, Double), _mouse :: Maybe (Double, Double), _clientsize :: (Int, Int) } deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | 
initOpenGL :: IO ()
initOpenGL = do
	diffuse  (Light 0) $= Color4  1.0 1.0 1.0 1.0
	position (Light 0) $= Vertex4 1.0 1.0 4.5 0.0

	light   (Light 0) $= Enabled -- TODO: Vertex colours seem to be ignored when lighting is enabled
	lighting          $= Enabled --

	depthFunc $= Just Lequal
	
	-- lineSmooth $= Enabled
	-- blend      $= Enabled
	-- blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
	
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
-- TODO: Improve control flow
-- TODO: Improve error checking (eg. which logs belong to which part, check errors at each stage?)
createShaderProgram :: String -> String -> IO (Either [String] Program)
createShaderProgram vsource psource = do
	program <- createProgram
	vshader <- createShader VertexShader
	pshader <- createShader FragmentShader

	shaderSourceBS vshader $= packUtf8 vsource
	shaderSourceBS pshader $= packUtf8 psource

	compileShader vshader
	compileShader pshader

	vstatus <- get $ compileStatus vshader
	pstatus <- get $ compileStatus pshader

	if vstatus && pstatus
		then do
			mapM (attachShader program) [vshader, pshader]
			linkProgram program
			linked <- get $ linkStatus program
			if linked
				then return $ Right program
				else mapM get [shaderInfoLog vshader, shaderInfoLog pshader, programInfoLog program] >>= return . Left
		else mapM (get . shaderInfoLog) [vshader, pshader] >>= return . Left


-- |
onmousedrag :: IORef AppState -> GLFW.CursorPosCallback
onmousedrag stateref _ mx my = do
	modifyIORef stateref $ \ state -> case state of
		AppState { _rotation=(rx, ry), _mouse=Just (mx', my') } -> state { _rotation=(rx+mx-mx', ry+my-my'), _mouse=Just (mx, my) }
		_                                                       -> state


--  |
-- TODO: Rename (?)
-- Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
onmousepress :: IORef AppState -> GLFW.MouseButtonCallback
onmousepress stateref _      MouseButton'1 MouseButtonState'Released _ = modifyIORef stateref $ releasemouse
onmousepress stateref window MouseButton'1 MouseButtonState'Pressed  _ = GLFW.getCursorPos window >>= modifyIORef stateref . movemouse
onmousepress _        _      _                  _                    _ = return ()


-- |
movemouse mouse state = state { _mouse=Just mouse }


-- |
releasemouse state = state { _mouse=Nothing }


-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize stateref _ cx cy = do
	modifyIORef stateref $ \ state -> state { _clientsize=(cx, cy) }     --
	-- viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --
	putStrLn $ unwords ["Window size is", show cx, "/", show cy]


-- |
render :: IORef AppState -> [Buffers] -> GLFW.WindowRefreshCallback
render stateref buffers window = do
	--
	(rx, ry)   <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref

	let (cols, rows) = (3, 3)

	gridM cols rows $ \ wxi wyi -> do
		let (cx, cy) = (cint cxi, cint cyi)
		    left     = (wxi*div cx cols)
		    top      = (wyi*div cy rows)
		    width    = (div cx cols)
		    height   = (div cy rows)
		-- TODO: Clear up this horrible mess (✓)
		clearColor $= Color4 (0.5*float wxi) (0.5*float wyi) (float wxi*float wyi*0.3) (1.0 :: GLfloat)
		viewport   $=      (Position left top, Size width height)
		scissor    $= Just (Position left top, Size width height)

		-- TODO: Refactor with preservingMatrix (?)
		matrixMode $= Modelview 0
		loadIdentity
		lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

		translate    ((Vector3 0.0 (-2.0) (-4.0)) :: Vector3 GLfloat)
		rotate (180.0 * (realToFrac rx/realToFrac cxi)) ((Vector3 0.0 1.0 0.0) :: Vector3 GLfloat)
		rotate (180.0 * (realToFrac ry/realToFrac cyi)) ((Vector3 1.0 0.0 0.0) :: Vector3 GLfloat)

		clear [ColorBuffer, DepthBuffer]
		forM_ buffers renderModel
	GLFW.swapBuffers window
	where float = realToFrac   -- TODO: Whuuuuuuut (monomorphism restriction)?
	      cint  = fromIntegral -- TODO: Ditto (?)


-- |
renderSimple window = do
	(width, height) <- GLFW.getFramebufferSize window
	let ratio = fromIntegral width / fromIntegral height
	
	viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
	clear [ColorBuffer]
	
	matrixMode $= Projection
	loadIdentity
	ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
	matrixMode $= Modelview 0
	
	loadIdentity
	-- this is bad, but keeps the logic of the original example I guess
	Just t <- GLFW.getTime
	rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)
	
	renderPrimitive Triangles $ do
		color  (Color3 1 0 0 :: Color3 GLdouble)
		vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
		color  (Color3 0 1 0 :: Color3 GLdouble)
		vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
		color  (Color3 0 0 1 :: Color3 GLdouble)
		vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
	GLFW.swapBuffers window

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
-- animate fps = do
	-- postRedisplay Nothing
	-- addTimerCallback (div 1000 fps) (animate fps)


---------------------------------------------------------------------------------------------------

-- | Like maybe, except the function comes last
perhaps :: b -> Maybe a -> (a -> b) -> b
perhaps fallback value action = maybe fallback action value


---------------------------------------------------------------------------------------------------
mainloop :: GLFW.Window -> IORef AppState -> [Buffers] -> IO ()
mainloop window stateref buffers = do
	-- renderSimple window
	render stateref buffers window
	GLFW.pollEvents
	closing <- GLFW.windowShouldClose window
	unless closing $ mainloop window stateref buffers


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

	True <- GLFW.init
	-- mmonitor <- GLFW.getPrimaryMonitor
	-- GLFW.defaultWindowHints -- TODO: Before or after window creation (?)
	mwindow  <- GLFW.createWindow cx cy "WaveFront OBJ Sample (2015)" Nothing Nothing

	perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
		-- initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling]
		putStrLn "Creating buffers..."
		let buffers = [createBuffers model]
		
		GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does

		-- GLFW.setWindowRefreshCallback window $ Just (render stateref buffers)
		GLFW.setCursorPosCallback     window $ Just (onmousedrag stateref)
		GLFW.setMouseButtonCallback   window $ Just (onmousepress stateref)
		GLFW.setWindowSizeCallback    window $ Just (onwindowresize stateref)
		-- GLFW.addTimerCallback (div 100 30) (animate 30)

		putStrLn "Finished creating buffers."
		initOpenGL

		-- GLFW.pollEvents -- mainLoop
		mainloop window stateref buffers
		GLFW.destroyWindow window
		GLFW.terminate

	putStrLn "Finished"
	mapM_ putStrLn ["Finished painting.",
					"Washing brushes...",
					"Mounting canvas...",
					"Disassembling easel...",
					"Done. Good bye!"]
