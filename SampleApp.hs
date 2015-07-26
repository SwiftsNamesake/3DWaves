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

-- TODO | - Break up the functionality of this program into several modules
--        - 

-- SPEC | -
--        -



module Southpaw.WaveFront.SampleApp where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Graphics.GLUtil

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW as GLFW (MouseButton(..), KeyState(..), MouseButtonState(..))

import System.FilePath (splitFileName, (</>))

import Control.Monad (forM_, liftM, unless)
import Control.Exception

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
type Buffers = [(Normal3 GLfloat, WF.Material, [Vertex3 GLfloat])]
data AppState = AppState { _rotation :: (Double, Double), _mouse :: Maybe (Double, Double), _clientsize :: (Int, Int) } deriving (Show)

data Mesh    = Mesh { vertices :: VBO,
	                  normals  :: VBO,
	                  colours  :: VBO}


---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | 
initOpenGL :: IO ()
initOpenGL = do
	--
	diffuse  (Light 0) $= Color4  1.0 1.0 1.0 1.0
	position (Light 0) $= Vertex4 1.0 1.0 4.5 0.0

	light   (Light 0) $= Enabled -- TODO: Vertex colours seem to be ignored when lighting is enabled
	lighting          $= Enabled --

	depthFunc $= Just Lequal

	-- Shader setup
	let path = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\Southpaw\\lib\\Southpaw\\WaveFront"
	eprogram <- loadShaderProgram (path </> "shader-vertex.glsl") (path </> "shader-pixel.glsl")
	print eprogram
	either
	  (\ logs    -> printf "Unable to load shader program: %s" $ show logs)
	  (\ program -> currentProgram $= Just program)
	  (eprogram)

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
-- TODO: Improve control flow
-- TODO: Improve error checking (eg. which logs belong to which part, check errors at each stage?)
-- TODO: Catch exceptions
-- TODO: Program crashes when the source strings are empty
-- TODO: Optional logging layer (?)
createShaderProgram :: String -> String -> IO (Either [String] Program)
createShaderProgram vsource psource = do
	putStrLn "Creating shader program"
	program <- createProgram
	vshader <- createShader VertexShader
	pshader <- createShader FragmentShader

	putStrLn "Setting vertex shader source"
	shaderSourceBS vshader $= packUtf8 vsource

	putStrLn "Compiling vertex shader"
	compileShader vshader

	putStrLn "Setting fragment shader source"
	shaderSourceBS pshader $= packUtf8 psource
	compileShader pshader

	-- putStrLn "Compiling shaders..."

	vstatus <- get $ compileStatus vshader
	print vstatus
	pstatus <- get $ compileStatus pshader
	print pstatus

	if vstatus && pstatus
		then do
			putStrLn "Successfully compiled shaders. Linking program..."
			mapM (attachShader program) [vshader, pshader]
			linkProgram program
			linked <- get $ linkStatus program
			if linked
				then return $ Right program
				else mapM get [shaderInfoLog vshader, shaderInfoLog pshader, programInfoLog program] >>= return . Left
		else mapM (get . shaderInfoLog) [vshader, pshader] >>= return . Left


--
setShaderUniforms :: Program -> IO ()
setShaderUniforms program = do
	-- activeUniforms
	let uMVMatrix = uniformLocation program "uMVMatrix" -- uniform mat4 uMVMatrix;
	    uPMatrix  = uniformLocation program "uPMatrix" -- uniform mat4 uPMatrix;

	uniform uMVMatrix $= undefined
	uniform uPMatrix  $= undefined

	return ()


-- |
loadShaderProgram :: String -> String -> IO (Either [String] Program)
loadShaderProgram vpath ppath = do
	vsource <- readFile vpath
	psource <- readFile ppath

	catch
	  (createShaderProgram vsource psource)            --
	  caught -- TODO: More elaborate exception message (?)
	where
	  caught :: IOException -> IO (Either [String] Program)
	  caught e = return $ Left ["Unable to open file."]


-- |
render :: IORef AppState -> [Buffers] -> GLFW.WindowRefreshCallback
render stateref buffers window = do
	--
	(rx, ry)   <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref

	let (cols, rows) = (3, 3)
	gridM cols rows $ \ wxi wyi -> do

		let (cx, cy) = (cint cxi, cint cyi)
		    left     = ((wxi-1)*div cx cols)
		    top      = ((wyi-1)*div cy rows)
		    width    = (div cx cols)
		    height   = (div cy rows)

		clearColor $= Color4 (min 1.0 $ 0.5*float (wxi-1)) (min 1.0 $ 0.5*float (wyi-1)) (min 1.0 $ float (wxi-1)*float (wyi-1)*0.3) (1.0 :: GLfloat)
		viewport   $=      (Position left top, Size width height)
		scissor    $= Just (Position left top, Size width height)

		-- TODO: Refactor with preservingMatrix (?)
		matrixMode $= Modelview 0
		loadIdentity
		lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

		translate    ((Vector3 0.0 (-2.0) (-4.0)) :: Vector3 GLfloat)
		rotate (180.0 * (float rx/float cxi)) ((Vector3 0.0 1.0 0.0) :: Vector3 GLfloat)
		rotate (180.0 * (float ry/float cyi)) ((Vector3 1.0 0.0 0.0) :: Vector3 GLfloat)

		clear [ColorBuffer, DepthBuffer]
		forM_ buffers renderModel
	GLFW.swapBuffers window
	where float :: (Real r, Fractional f) => r -> f -- TODO: Whuuuuuuut (monomorphism restriction)?
	      float = realToFrac   

	      cint :: (Num n,  Integral i) => i -> n -- TODO: Ditto (?)
	      cint = fromIntegral


-- |
-- TODO: Colours and textures
-- TODO: Arbitrary polygons
-- TODO: Refactor, simplify
renderModel :: Buffers -> IO ()
renderModel buffers = forM_ buffers renderFace


-- | 
-- TODO: Use index buffer (?)
createMesh :: Model -> IO Mesh
createMesh model = do
	vertices <- makeBuffer ArrayBuffer $ concat [ [x, y, z] | (x, y, z) <- WF.vertices model]
	colours  <- makeBuffer ArrayBuffer $
	normals  <- makeBuffer ArrayBuffer $


-- |
renderMesh :: Mesh -> IO ()
renderMesh mesh = do
	drawArrays TriangleFan 0 (3*0) --
	return ()
-- drawArrays :: PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO ()
-- bindVertexArrayObject :: StateVar (Maybe VertexArrayObject)
-- 

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


-- Events -----------------------------------------------------------------------------------------

-- | 
-- animate fps = do
	-- postRedisplay Nothing
	-- addTimerCallback (div 1000 fps) (animate fps)


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
