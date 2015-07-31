-- |
-- Module      : Southpaw.WaveFront.SampleApp
-- Description : Executable which loads a model and displays it using OpenGL
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 15 2015

-- TODO | - Break up the functionality of this program into several modules
--        - FPS
--        - Cleanup

-- SPEC | -
--        -



module Southpaw.WaveFront.SampleApp where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
-- import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.GLUtil hiding        (loadShaderProgram)
import Graphics.GLUtil.JuicyTextures (readTexture)

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
import qualified Southpaw.Michelangelo.Mesh    as Mesh    -- 
import           Southpaw.Michelangelo.Mesh (Mesh(..))    -- 




---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- | 
type Buffers = [(Normal3 GLfloat, WF.Material, [Vertex3 GLfloat])]
data AppState = AppState { _rotation :: (Double, Double), _mouse :: Maybe (Double, Double), _clientsize :: (Int, Int), _frame :: Int } deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | 
initOpenGL :: IO ()
initOpenGL = do

	--
	showOpenGLInformation

	--
	depthFunc $= Just Lequal
	-- blend      $= Enabled
	-- blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)


-- |
createPrograms :: IO (Either String [Program])
createPrograms = do
	-- Shader setup
	-- TODO: Load all shaders from a given path (return Map) (?)
	let path = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\Southpaw\\lib\\Southpaw\\Michelangelo\\shaders"
	eprogram <- mapM (\ (vs, ps) -> Shaders.loadShaderProgram (path </> vs) (path </> ps)) [("shader-vertex.glsl",         "shader-pixel.glsl"),
	                                                                                        ("shader-textured-pixel.glsl", "shader-textured-vertex.glsl")]

	-- TODO: More detailed error message (?)
	return $ eprogram


-- |
render :: IORef AppState -> [Mesh] -> GLFW.WindowRefreshCallback
render stateref meshes window = do
	--
	(rx, ry)   <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref
	frame      <- liftM _frame      . readIORef $ stateref

	modifyIORef stateref $ \state@(AppState { _frame=n }) -> state { _frame=(n+1) }

	let (cols, rows) = (1, 1)
	gridM cols rows $ \ wxi wyi -> do

		let (cx, cy) = (cint cxi, cint cyi)
		    left     = ((wxi-1)*div cx cols)
		    top      = ((wyi-1)*div cy rows)
		    width    = (div cx cols)
		    height   = (div cy rows)

		-- clearColor $= Color4 (min 1.0 $ 0.5*float (wxi-1)) (min 1.0 $ 0.5*float (wyi-1)) (min 1.0 $ float (wxi-1)*float (wyi-1)*0.3) (1.0 :: GLfloat)
		clearColor $= Color4 (0.2) (0.72) (0.23) (1.0)
		viewport   $= (Position left top, Size width height)
		-- scissor    $= Just (Position left top, Size width height)

		-- (V3 0.0 (6*float ry/float cyi) (6*float rx/float cxi)) (V3 0 (2 * pi * float ry/float cyi) 0)
		clear [ColorBuffer, DepthBuffer]
		forM_ meshes (Mesh.renderMesh)
	GLFW.swapBuffers window
	throwError

	where
	  float :: (Real r, Fractional f) => r -> f -- TODO: Whuuuuuuut (monomorphism restriction)?
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
-- TODO: Use index buffer (?)
-- TODO: Texture support
-- TODO: More flexible treatment of 'programs'
createMesh :: Program -> WF.Model -> IO Mesh.Mesh
createMesh program model = do
	putStrLn "Creating mesh from model"
	case texcoords of
		[] -> createTexturedMesh program (vertices, texcoords) --
		_  -> createPaintedMesh  program (vertices, ms)        --  
	where
	  (vs, ts, ns, ms) = WF.modelAttributes model
	  texcoords        = concat $ ([ map realToFrac [x, y]    | (x, y)    <- catMaybes ts] :: [[CFloat]]) -- 
	  vertices         = concat $ ([ map realToFrac [x, y, z] | (x, y, z) <- vs]           :: [[CFloat]]) -- 


-- |
createTexturedMesh :: Program -> ([CFloat], [CFloat]) -> IO Mesh.Mesh
createTexturedMesh program (vs, ts) = do

	vertexbuffer   <- makeBuffer ArrayBuffer $ vs -- 
	texcoordbuffer <- makeBuffer ArrayBuffer $ ts --  

	locv <- get $ attribLocation program "aVertexPosition" -- 
	loct <- get $ attribLocation program "aTexCoord"       -- 
	-- locc <- get $ attribLocation program "aVertexColor" -- 

	-- TODO: Initialise properly
	return Mesh.Mesh { Mesh.attributes=Map.fromList [("aVertexPosition", (locv, vertexbuffer,   3)),
	                                                 ("aTexCoord",       (loct, texcoordbuffer, 2))],

	                   Mesh.primitive = Triangles,
	                   Mesh.texture   = Nothing,
	                   Mesh.shader    = program,
	                   Mesh.uniforms  = Map.empty,
	                   Mesh.size      = length $ vs }


-- |
-- TODO: Use type synomyms
createPaintedMesh :: Program -> ([CFloat], [WF.Material]) -> IO Mesh.Mesh
createPaintedMesh program (vs, cs) = do
	locv <- get $ attribLocation program "aVertexPosition" -- 
	locc <- get $ attribLocation program "aVertexColor"    -- 

	vertexbuffer <- makeBuffer ArrayBuffer $ vs
	colourbuffer <- makeBuffer ArrayBuffer . concat $ ([ map realToFrac [r, g, b, a] | (r, g, b, a) <- map WF.diffuse cs] :: [[CFloat]]) -- 

	-- TODO: Initialise properly
	return Mesh.Mesh { Mesh.attributes=Map.fromList [("aVertexPosition", (locv, vertexbuffer, 3)),
	                                                 ("aVertexColor",    (locc, colourbuffer, 4))],

	                   Mesh.primitive = Triangles,
	                   Mesh.texture   = Nothing,
	                   Mesh.shader    = program,
	                   Mesh.uniforms  = Map.empty,
	                   Mesh.size      = length $ vs }


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
-- TODO: Move to separate module (eg. loops/control structures) (?)
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
mainloop :: GLFW.Window -> IORef AppState -> [Mesh] -> IO ()
mainloop window stateref meshes = do
	-- renderSimple window
	render stateref meshes window
	GLFW.pollEvents
	closing <- GLFW.windowShouldClose window
	unless closing $ mainloop window stateref meshes

---------------------------------------------------------------------------------------------------

-- |
showOpenGLInformation :: IO ()
showOpenGLInformation = do
	printf "\n==== Version information =================================================\n"
	get vendor    >>= printf "Vendor:   %s\n"
	get renderer  >>= printf "Renderer: %s\n"
	get glVersion >>= printf "Version:  %s\n"
	get shadingLanguageVersion >>= printf "GLSL Version: %s\n"
	printf "==========================================================================\n\n"

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
		mapM option $ zip ([1..] :: [Int]) paths
		choice <- untilM (valid paths) (const $ prompt "That doesn't work. Try again: ") (prompt "Choose one: ")
		return $ case choice of
			Just index -> Right $ paths !! (index-1) -- 
			Nothing    -> Left  $ "Invalid choice"   -- This should never happen, throw error instead (?)
	where
		valid paths = return . maybe False (clamped 0 (length paths) . (subtract 1)) -- Is th
		prompt q    = putStr q >> hFlush stdout >> (liftM readMaybe) getLine         -- Ask for input (flush is sometimes required when q doesn't end in a newline)
		possibly f x g   = either f g x                                              -- Do-block at the end instead of in the middle
		option (n, path) = printf "  [%d] %s\n" n path                               -- Prints a model option



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
	stateref <- newIORef AppState { _rotation=(0,0), _mouse=Nothing, _clientsize=(720, 480), _frame=1 } --
	(cx, cy) <- liftM _clientsize . readIORef $ stateref                                                -- Yes, I know this is stupid.

	True <- GLFW.init
	-- mmonitor <- GLFW.getPrimaryMonitor
	-- GLFW.defaultWindowHints -- TODO: Before or after window creation (?)
	GLFW.windowHint $ GLFW.WindowHint'Samples 4
	mwindow  <- GLFW.createWindow cx cy "WaveFront OBJ Sample (2015)" Nothing Nothing

	perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
		-- initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling]
		GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does

		-- GLFW.setWindowRefreshCallback window $ Just (render stateref buffers)
		GLFW.setMouseButtonCallback window $ Just (onmousepress stateref)
		GLFW.setCursorPosCallback   window $ Just (onmousedrag stateref)
		GLFW.setWindowSizeCallback  window $ Just (onwindowresize stateref)
		-- GLFW.setDropCallback      window $ Nothing
		-- GLFW.setCharCallback
		-- GLFW.setKeyCallback
		-- GLFW.setErrorCallback
		-- GLFW.addTimerCallback (div 100 30) (animate 30)

		initOpenGL
		eprograms <- createPrograms		

		-- GLFW.pollEvents -- mainLoop
		either
		  (\ err      -> printf "Sumtin baay-uhd happened (%s). Bailing out...\n" err)
		  (\ programs -> do
		    meshes <- mapM (createMesh programs) [model]
		    mainloop program window stateref meshes)
		  (eprograms)


		GLFW.destroyWindow window
		GLFW.terminate

	putStrLn "Finished"
	mapM_ putStrLn ["Finished painting.",
					"Washing brushes...",
					"Mounting canvas...",
					"Disassembling easel...",
					"Done. Good bye!"]
