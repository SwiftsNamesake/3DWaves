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
-- import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.GLUtil hiding (loadShaderProgram)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW as GLFW (MouseButton(..), KeyState(..), MouseButtonState(..))

import Linear.Matrix
import Linear.V3

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  (splitFileName, splitExtension, (</>))
import System.IO        (hFlush, stdout)

import Control.Monad (forM_, liftM, unless)
import Control.Exception

import Foreign.Ptr
import Foreign.C.Types
-- import qualified Data.Vector.Storable as V

import Data.Maybe    (catMaybes)
import Data.Either   (rights, lefts)
import qualified Data.Map as Map

import Text.Printf (printf)
import Text.Read   (readMaybe)

import Data.IORef

import Southpaw.Utilities.Utilities (numeral, gridM)

import qualified Southpaw.WaveFront.Parsers    as WF      -- (loadModel, loadOBJ, loadMTL, facesOf, Model(..), Face(..), OBJToken(..), Material(..))
import qualified Southpaw.WaveFront.Load       as WFL     --
import qualified Southpaw.Michelangelo.Shaders as Shaders --




---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- | 
type Buffers = [(Normal3 GLfloat, WF.Material, [Vertex3 GLfloat])]
data AppState = AppState { _rotation :: (Double, Double), _mouse :: Maybe (Double, Double), _clientsize :: (Int, Int) } deriving (Show)

data Mesh    = Mesh { vertices  :: BufferObject,
	                  colours   :: BufferObject,
	                  normals   :: Maybe BufferObject,
	                  texcoords :: Maybe BufferObject,
	                  size      :: Int}
	           deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | 
initOpenGL :: IO (Maybe Program)
initOpenGL = do
	--
	depthFunc $= Just Lequal

	printError

	-- Shader setup
	let path = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\Southpaw\\lib\\Southpaw\\WaveFront"
	eprogram <- Shaders.loadShaderProgram (path </> "shader-vertex.glsl") (path </> "shader-pixel.glsl")

	printError

	mprogram <- either
	  (\ logs    -> printf "Unable to load shader program: %s" (show logs) >> return Nothing)
	  (\ program -> currentProgram $= Just program >> return (Just program))
	  (eprogram)

	printError

	_ <- perhaps (return ()) mprogram $ \program -> do
		putStrLn "Attribute setup"
		attribLocation program "aVertexPosition" $= AttribLocation 0
		attribLocation program "aVertexColor"    $= AttribLocation 1

		printError

		vertexAttribArray (AttribLocation 0) $= Enabled
		vertexAttribArray (AttribLocation 1) $= Enabled

		printError
	-- lineSmooth $= Enabled
	-- blend      $= Enabled
	-- blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
	
	-- matrixMode  $= Projection
	-- perspective 40.0 1.0 1.0 10.0

	printError

	-- matrixMode $= Modelview 0
	-- lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

	throwError

	-- translate    ((Vector3 0.0 0.0 (-4.0)) :: Vector3 GLfloat)
	-- rotate (-30) ((Vector3 1.0 0.0   0.0)  :: Vector3 GLfloat)
	-- rotate (-20) ((Vector3 0.0 0.0   1.0)  :: Vector3 GLfloat)

	throwError

	-- scale (0.05 :: GLfloat) 0.5 0.5
	return mprogram


-- |
render :: Program -> IORef AppState -> [Mesh] -> GLFW.WindowRefreshCallback
render program stateref meshes window = do
	--
	(rx, ry)   <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref

	let (cols, rows) = (2, 2)
	gridM cols rows $ \ wxi wyi -> do

		let (cx, cy) = (cint cxi, cint cyi)
		    left     = ((wxi-1)*div cx cols)
		    top      = ((wyi-1)*div cy rows)
		    width    = (div cx cols)
		    height   = (div cy rows)

		clearColor $= Color4 (min 1.0 $ 0.5*float (wxi-1)) (min 1.0 $ 0.5*float (wyi-1)) (min 1.0 $ float (wxi-1)*float (wyi-1)*0.3) (1.0 :: GLfloat)
		viewport   $=      (Position left top, Size width height)
		scissor    $= Just (Position left top, Size width height)

		throwError

		-- TODO: Refactor with preservingMatrix (?)
		-- matrixMode $= Modelview 0
		-- loadIdentity
		-- lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

		throwError

		-- translate ((Vector3 0.0 (-2.0) (-4.0)) :: Vector3 GLfloat)
		-- rotate (180.0 * (float rx/float cxi)) ((Vector3 0.0 1.0 0.0) :: Vector3 GLfloat)
		-- rotate (180.0 * (float ry/float cyi)) ((Vector3 1.0 0.0 0.0) :: Vector3 GLfloat)

		throwError

		clear [ColorBuffer, DepthBuffer]
		forM_ meshes (renderMesh program (V3 0.0 (6*float ry/float cyi) (6*float rx/float cxi)) (V3 0 0 0))
	GLFW.swapBuffers window
	throwError

	where float :: (Real r, Fractional f) => r -> f -- TODO: Whuuuuuuut (monomorphism restriction)?
	      float = realToFrac   

	      cint :: (Num n,  Integral i) => i -> n -- TODO: Ditto (?)
	      cint = fromIntegral


-- |
-- TODO: Colours and textures
-- TODO: Arbitrary polygons
-- TODO: Refactor, simplify
-- renderModel :: Buffers -> IO ()
-- renderModel buffers = forM_ buffers renderFace


-- | 
-- TODO: Use index buffer (  ?)
createMesh :: WF.Model -> IO Mesh
createMesh model = do
	putStrLn "Creating mesh from model"
	vertices  <- makeBuffer ArrayBuffer . concat $ ([ map realToFrac [x, y, z]    | (x, y, z)    <- vs] :: [[CFloat]])                        -- 
	normals   <- liftM Just . makeBuffer ArrayBuffer . concat $ ([ map realToFrac [x, y, z]    | (x, y, z)    <- catMaybes ns] :: [[CFloat]]) -- 
	texcoords <- return Nothing -- makeBuffer ArrayBuffer . concat $ [ [x, y]       | (x, y)       <- catMaybes ts]                           -- 
	colours   <- makeBuffer ArrayBuffer . concat $ ([ map realToFrac [r, g, b, a] | (r, g, b, a) <- map WF.diffuse ms] :: [[CFloat]])         -- 
	printError
	return Mesh { vertices  = vertices,
	              normals   = normals,
	              texcoords = texcoords,
	              colours   = colours,
	              size      = length vs}
	where (vs, ts, ns, ms) = WF.modelAttributes model


-- |
renderMesh :: Program -> V3 Float -> V3 Float -> Mesh -> IO ()
renderMesh program (V3 tx ty tz) (V3 rx ry rz) (Mesh { vertices=v, normals=n, texcoords=t, colours=c, size=s }) = do
	-- putStrLn "Rendering mesh"
	-- TODO: Don't hard-code attribute locations
	-- TODO: I probably shouldn't be looking up uniform and attribute locations with each frame render...
	-- attributes <- get (activeAttribs program)
	-- let vertexAttrib = AttribLocation program _ -- $ head [ pos | (pos, _, "aVertexPosition") <- attributes ]
	-- let colourAttrib = AttribLocation $ head [ pos | (pos, _, "aVertexColor") <- attributes ]
	-- attribLocation program "aVertexPosition" $= AttribLocation 0
	-- attribLocation program "aVertexColor"    $= AttribLocation 3

	-- throwError

	-- vertexAttribArray (AttribLocation 0) $= Enabled
	-- vertexAttribArray (AttribLocation 3) $= Enabled
	-- vertexAttribArray (AttribLocation 2) $= Enabled
	vertexattrib <- get $ attribLocation program "aVertexPosition"
	colourattrib <- get $ attribLocation program "aVertexColor"


	vertexAttribArray (AttribLocation 0) $= Enabled
	vertexAttribArray (AttribLocation 1) $= Enabled

	bindBuffer ArrayBuffer $= Just v
	--(plusPtr nullPtr 0)
	vertexAttribPointer (vertexattrib) $= (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)

	throwError

	-- bindBuffer ArrayBuffer $= Just n
	-- vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)
	
	throwError

	bindBuffer ArrayBuffer $= Just c
	vertexAttribPointer (colourattrib) $= (ToFloat, VertexArrayDescriptor 4 Float 0 offset0)

	Shaders.setShaderUniforms program (V3 tx ty tz) (V3 rx ry rz)
	throwError
	drawArrays TriangleFan 0 (fromIntegral $ s) --
	throwError

	vertexAttribArray (AttribLocation 0) $= Disabled
	vertexAttribArray (AttribLocation 1) $= Disabled

-- drawArrays :: PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO ()
-- bindVertexArrayObject :: StateVar (Maybe VertexArrayObject)


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
onmousepress _        _      _             _                         _ = return ()


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


-- | Monadic ternary operator
-- TOOD: Rename
assumingM :: Monad m => m Bool -> m a -> m a -> m a
assumingM p a b = p >>= \ done -> if done then a else b


-- | Perform an action until the returned value satisfies the condition
-- TODO: Pass in previous result (✓)
-- TODO: Refactor, rename variables (?)
untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f x = do
	value <- x 
	done  <- p value
	if done
	  then return value
	  else untilM p f (f value)


-- | a ∈ [low, upp]
clamped low upp a = (low <= a) && (a <= upp) 


---------------------------------------------------------------------------------------------------

-- |
mainloop :: Program -> GLFW.Window -> IORef AppState -> [Mesh] -> IO ()
mainloop program window stateref meshes = do
	-- renderSimple window
	render program stateref meshes window
	GLFW.pollEvents
	closing <- GLFW.windowShouldClose window
	unless closing $ mainloop program window stateref meshes

---------------------------------------------------------------------------------------------------


-- |
listOBJFiles :: String -> IO (Either String [String])
listOBJFiles path = do
	exists <- doesDirectoryExist path
	if exists
	  then getDirectoryContents path >>= return . Right . map (path </>) . filter ((==".obj") . snd . splitExtension)
	  else return (Left "No such directory")


-- |
-- TODO: Console cursor (?)
chooseModelsFrom :: String -> IO (Either String String)
chooseModelsFrom path = do
	epaths <- listOBJFiles path
	possibly (\_ -> return $ Left "No such directory") epaths $ \paths -> do
		putStrLn "Which model would you like to load?"
		mapM (\ (n, path) -> printf "  [%d] %s\n" n path) $ zip ([0..] :: [Int]) paths
		Just choice <- untilM (valid paths) (const $ prompt "That doesn't work. Try again: ") (prompt "Choose one: ")
		return $ Right (paths !! choice)
	where
		valid paths = return . maybe False (clamped 0 (length paths))        --
		prompt q    = putStr q >> hFlush stdout >> (liftM readMaybe) getLine --
		possibly f x g = either f g x                                        --



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
	--
	let path = "C:/Users/Jonatan/Desktop/3D/models/"
	modelname <- chooseModelsFrom path >>= either (\_ -> putStrLn "Something went wrong. Using default model." >> return (path </> "king.obj")) return
	putStrLn "Loading model..."
	model <- WFL.loadModel $ modelname
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
		GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does

		-- GLFW.setWindowRefreshCallback window $ Just (render stateref buffers)
		GLFW.setCursorPosCallback     window $ Just (onmousedrag stateref)
		GLFW.setMouseButtonCallback   window $ Just (onmousepress stateref)
		GLFW.setWindowSizeCallback    window $ Just (onwindowresize stateref)
		-- GLFW.addTimerCallback (div 100 30) (animate 30)

		putStrLn "Creating buffers..."
		mprogram <- initOpenGL
		meshes   <- mapM createMesh [model]
		putStrLn "Finished creating buffers."

		-- GLFW.pollEvents -- mainLoop
		maybe
		  (putStrLn "Sumtin baay-uhd happened. Bailing out...")
		  (\program -> mainloop program window stateref meshes)
		  (mprogram)

		GLFW.destroyWindow window
		GLFW.terminate

	putStrLn "Finished"
	mapM_ putStrLn ["Finished painting.",
					"Washing brushes...",
					"Mounting canvas...",
					"Disassembling easel...",
					"Done. Good bye!"]
