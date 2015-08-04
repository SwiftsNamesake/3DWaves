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
import Graphics.Rendering.OpenGL as GL hiding (perspective, ortho, rotate, translate, scale)
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
-- import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.Rendering.FTGL as FTGL --

import Graphics.GLUtil hiding        (loadShaderProgram)
import Graphics.GLUtil.JuicyTextures (readTexture)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW as GLFW (MouseButton(..), MouseButtonState(..))

import Linear.Matrix
import Linear.V3
import Linear.Projection
-- import Linear.Quaternion

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  (splitExtension, (</>))
import System.IO        (hFlush, stdout)

import Control.Monad (forM_, liftM, unless)
import Control.Lens
import Control.Concurrent
-- import Control.Exception

-- import Foreign.Ptr
import Foreign.C.Types
-- import qualified Data.Vector.Storable as V

import Data.Maybe    (catMaybes)
-- import Data.Either   (rights, lefts)
import qualified Data.Map as Map

import Text.Printf (printf)
import Text.Read   (readMaybe)

import Data.IORef
import qualified Data.Set as Set

import Southpaw.Utilities.Utilities (gridM, radians) --, π)

import qualified Southpaw.WaveFront.Parsers    as WF      -- (loadModel, loadOBJ, loadMTL, facesOf, Model(..), Face(..), OBJToken(..), Material(..))
import qualified Southpaw.WaveFront.Load       as WFL     -- 

import           Southpaw.Michelangelo.Transformations    --
import           Southpaw.Michelangelo.Mesh (Mesh(..))    -- 
import qualified Southpaw.Michelangelo.Mesh    as Mesh    -- 
import qualified Southpaw.Michelangelo.Shaders as Shaders -- 
import           Southpaw.Michelangelo.Shaders (UniformValue(..)) -- 



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
	-- depthTest $= Enabled
	depthFunc $= Just Lequal
	blend     $= Enabled
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

	--
	-- GL.texture Texture2D $= Enabled   -- TODO: Move this (?)
	-- GL.activeTexture $= TextureUnit 0 --

	-- textureBinding Texture2D $= Just texName0
	textureWrapMode Texture2D S $= (Repeated, Repeat)
	textureWrapMode Texture2D T $= (Repeated, Repeat)
	textureFilter Texture2D $= ((Nearest, Nothing), Nearest)


-- |
loadPrograms :: IO (Either [String] [Program])
loadPrograms = do
	-- Shader setup
	-- TODO: Load all shaders from a given path (return Map) (?)
	let path = "C:/Users/Jonatan/Desktop/Haskell/modules/Southpaw/lib/Southpaw/Michelangelo/shaders"
	eprogram <- mapM (\ (vs, ps) -> Shaders.loadShaderProgram (path </> vs) (path </> ps)) [("shader-vertex.glsl",          "shader-pixel.glsl"),
	                                                                                        ("shader-textured-vertex.glsl", "shader-textured-pixel.glsl")]

	-- TODO: More detailed error message (?)
	return $ sequence eprogram


-- |
render :: IORef AppState -> [Mesh] -> GLFW.WindowRefreshCallback
render stateref meshes window = do
	--
	(rx, ry)   <- liftM _rotation   . readIORef $ stateref
	(cxi, cyi) <- liftM _clientsize . readIORef $ stateref
	-- frame      <- liftM _frame      . readIORef $ stateref

	modifyIORef stateref $ \state@(AppState { _frame=n }) -> state { _frame=(n+1) }

	viewport $= (Position 0 0, Size (cint cxi) (cint cyi))
	clearColor $= Color4 (0.2) (0.72) (0.23) (1.0)
	clear [ColorBuffer, DepthBuffer]

	let (cols, rows) = (5,5) :: (Int, Int)
	gridM cols rows $ \ wxi wyi -> do
		let (cx, cy) = (cint cxi, cint cyi)
		    left     = ((wxi-1)*div cx cols)
		    top      = ((wyi-1)*div cy rows)
		    width    = (div cx cols)
		    height   = (div cy rows)
		    (x, y, z) = (8.5*float (wxi-1), 8.5*float (wyi-1) + 6*float ry/float cyi, 6*float rx/float cxi)
		    modelview = (identity) & (translation .~ ((V3 x y z) - (Mesh.centre $ head meshes)))
		    setModelview mesh = mesh { Mesh.uniforms=Map.update (\(loc, _) -> Just (loc, UMatrix44 modelview)) "uMVMatrix" (Mesh.uniforms mesh) }

		-- clearColor $= Color4 (min 1.0 $ 0.5*float (wxi-1)) (min 1.0 $ 0.5*float (wyi-1)) (min 1.0 $ float (wxi-1)*float (wyi-1)*0.3) (1.0 :: GLfloat)
		-- viewport   $= (Position left top, Size width height)
		-- scissor    $= Just (Position left top, Size width height)

		-- TODO: Use lenses, move animation logic to separate function (cf. Entity)
		-- (V3 0.0 (6*float ry/float cyi) (6*float rx/float cxi)) (V3 0 (2 * pi * float ry/float cyi) 0)
		-- let rotatation = rotateY ((2*π * float ry/float cyi) :: Float)
		-- let modelview = (identity) & (translation .~ (V3 0.0 0.0 (4.0)))
		-- print (V3 0.0 (6*float ry/float cyi) (6*float rx/float cxi))
		forM_ meshes (Mesh.renderMesh . setModelview)

	-- Text
	-- helloworld
	-- color3f (Color4 1.0 0.2 1.0 (1.0 :: Float))
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
helloworld :: IO ()
helloworld = do
	font <- FTGL.createTextureFont "FTLTLT.TTF"
	FTGL.setFontFaceSize font 24 72
	FTGL.renderFont font "Hola Mundo!" FTGL.Front


-- | 
-- TODO: Use index buffer (?)
-- TODO: Texture support
-- TODO: More flexible treatment of 'programs'
createMesh :: (WF.Model -> Program) -> WF.Model -> IO Mesh.Mesh
createMesh chooseProgram model = do

	putStrLn "Creating mesh from model"

	let bounds = WF.boundingbox model
	    width  = WF.right  bounds - WF.left bounds
	    height = WF.top    bounds - WF.bottom bounds
	    depth  = WF.front  bounds - WF.back bounds
	    cx     = WF.left   bounds + width  / 2
	    cy     = WF.bottom bounds + height / 2
	    cz     = WF.back   bounds + depth  / 2

	printf "Width=%.02f, Height=%.02f, Depth=%.02f, Centre=(%.02f, %.02f, %.02f)\n" width height depth cx cy cz
	printf "TS: %s\n" . show $  take 10 (catMaybes ts)
	printf "VS: %s\n" . show $  take 10 (vs)
	printf "VS: %s\n" . show $  take 10 (WF.vertices model)
	if WF.hasTextures model
		then createTexturedMesh (chooseProgram model) (vertices, texcoords) (model) (V3 cx cy cz) --
		else createPaintedMesh  (chooseProgram model) (vertices, ms)        (model) (V3 cx cy cz) --  
	where
	  (vs, ts, _, ms) = WF.modelAttributes model
	  texcoords        = concat $ ([ map realToFrac [x, y]    | (x, y)    <- catMaybes ts] :: [[CFloat]]) -- 
	  vertices         = concat $ ([ map realToFrac [x, y, z] | (x, y, z) <- vs]           :: [[CFloat]]) -- 
	  -- normals          = concat $ ([ map realToFrac [xs, y, z] | (x, y, z) <- ns]           :: [[CFloat]]) -- 


-- |
-- TODO: Wrap in Maybe (eg. for missing textures) (?)
-- TODO: Don't hard-code texture path
-- TODO: Default to painted when the textures fail to load (?)
createTexturedMesh :: Program -> ([CFloat], [CFloat]) -> WF.Model -> V3 Float -> IO Mesh.Mesh
createTexturedMesh theprogram (vs, ts) model centre = do
	
	putStrLn "Creating textured mesh"
	vertexbuffer   <- makeBuffer ArrayBuffer $ vs -- 
	texcoordbuffer <- makeBuffer ArrayBuffer $ ts -- 
	putStrLn "Finished creating mesh buffers"

	[locv, loct] <- mapM (get . attribLocation theprogram) [("aVertexPosition"), ("aTexCoord")] -- 

	printError
	etextures <- liftM sequence $ mapM (readTexture . ("C:\\Users\\Jonatan\\Desktop\\3D\\models\\textures" </>)) (Set.toList $ WF.textures model) -- TODO: This could fail
	printError

	print (etextures)

	mtexture <- either
	  (\err     -> printf "Failed to load textures (%s) for mesh: %s.\n" (show $ WF.textures model) err >> return Nothing)
	  (\(tex:_) -> return $ Just tex)
	  (etextures)

	-- Uniforms
	-- TODO: Use activeTexture (?)
	locs     <- get $ uniformLocation theprogram "uSampler"
	uniforms <- liftM (Map.fromList . (++ [("uSampler", (locs, UInt 0))])) $ defaultMatrixUniforms theprogram

	printError

	printf "Sampler location: %s\n" (show locs)

	-- TODO: Initialise properly
	return Mesh.Mesh { Mesh.attributes=Map.fromList [("aVertexPosition", (locv, vertexbuffer,   3)),
	                                                 ("aTexCoord",       (loct, texcoordbuffer, 2))],

	                   Mesh.primitive = Triangles,
	                   Mesh.texture   = mtexture,   -- TOOD: This could fail
	                   Mesh.shader    = theprogram,
	                   Mesh.uniforms  = uniforms,
	                   Mesh.prepare   = Just prepare,
	                   Mesh.centre    = centre,
	                   Mesh.bounds    = WF.boundingbox model,
	                   Mesh.size      = (length $ vs) `div` 3 }
	where
	  prepare mesh = do
	    --
	    GL.currentProgram $= Just (Mesh.shader mesh)

	    -- Texturing
	    -- GL.texture GL.Texture2D $= GL.Enabled -- TODO: Move this (?)
	    GL.activeTexture $= (GL.TextureUnit 0)                --
	    -- print (Mesh.texture mesh)
	    GL.textureBinding GL.Texture2D $= (Mesh.texture mesh) -- Is this needed (?)
	    -- texture2DWrap $= (Repeated, ClampToEdge)
	    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
	    textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
	    textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)



-- |
-- TODO: Use type synomyms
createPaintedMesh :: Program -> ([CFloat], [WF.Material]) -> WF.Model -> V3 Float -> IO Mesh.Mesh
createPaintedMesh theprogram (vs, cs) model centre = do

	putStrLn "Creating painted mesh"

	-- Attributes
	[locv, locc] <- mapM (get . attribLocation theprogram) ["aVertexPosition", "aVertexColor"] --

	vertexbuffer <- makeBuffer ArrayBuffer $ vs
	colourbuffer <- makeBuffer ArrayBuffer . concat $ ([ map realToFrac [r, g, b, a] | (r, g, b, a) <- map WF.diffuse cs] :: [[CFloat]]) -- 

	-- Uniforms
	uniforms <- liftM Map.fromList $ defaultMatrixUniforms theprogram

	-- TODO: Initialise properly
	return Mesh.Mesh { Mesh.attributes=Map.fromList [("aVertexPosition", (locv, vertexbuffer, 3)),
	                                                 ("aVertexColor",    (locc, colourbuffer, 4))],

	                   Mesh.primitive = Triangles,
	                   Mesh.texture   = Nothing,
	                   Mesh.shader    = theprogram,
	                   Mesh.uniforms  = uniforms,
	                   Mesh.prepare   = Just (\mesh -> GL.currentProgram $= Just (Mesh.shader mesh)),
	                   Mesh.centre    = centre,
	                   Mesh.bounds    = WF.boundingbox model,
	                   Mesh.size      = (length $ vs) `div` 3 }


-- |
-- TODO: This is quite fragile at the moment
defaultMatrixUniforms :: Program -> IO [(String, (GL.UniformLocation, UniformValue))]
defaultMatrixUniforms theprogram = do
	[locmv, locmp] <- mapM (get . uniformLocation theprogram) ["uMVMatrix", "uPMatrix"]

	-- TODO: Experiment with inverse perspective
	let modelview  = (rotateY (0.0 :: Float) !*! identity) & (translation .~ (V3 0 0 0))
	let projection = (perspective
	                   (radians 40.0) -- FOV (y direction, in radians)
	                   1.0            -- Aspect ratio
	                   1.0            -- Near plane
	                   80.0)          -- Far plane

	return [("uMVMatrix", (locmv, UMatrix44 modelview)), ("uPMatrix", (locmp, UMatrix44 projection))]


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
		valid paths = return . maybe False (clamped 0 (length paths) . (subtract 1)) -- 
		prompt q    = putStr q >> hFlush stdout >> (liftM readMaybe) getLine         -- Ask for input (flush is sometimes required when q doesn't end in a newline)
		possibly f x g   = either f g x                                              -- Do-block at the end instead of in the middle
		option (n, path) = printf "  [%d] %s\n" n path                               -- Prints a model option



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- |
openGLMain :: IO ()
openGLMain = do
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

		initOpenGL                --
		eprograms <- loadPrograms -- TODO: Don't make assumptions about the order of shaders		

		-- GLFW.pollEvents -- mainLoop
		either
		  (\ err                 -> printf "Sumtin baay-uhd happened (%s). Bailing out...\n" (unlines err))
		  (\ [painted, textured] -> do
		    meshes <- mapM (createMesh (\ _ -> if WF.hasTextures model then textured else painted)) [model]
		    mainloop window stateref meshes)
		  (eprograms)


		GLFW.destroyWindow window
		GLFW.terminate

	putStrLn "Finished"
	mapM_ putStrLn ["Finished painting.",
					"Washing brushes...",
					"Mounting canvas...",
					"Disassembling easel...",
					"Done. Good bye!"]


-- |
main :: IO ()
main = do
	openGLMain