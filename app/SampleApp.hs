-- |
-- Module      : SampleApp - Main
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



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NamedFieldPuns         #-}


--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Vector as V
import           Data.Vector ((!?))
import           Data.Int    (Int64)

import Graphics.Rendering.OpenGL as GL hiding (perspective, ortho, rotate, translate, scale)
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
-- import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

-- import Graphics.Rendering.FTGL as FTGL --

import Graphics.GLUtil hiding        (loadShaderProgram)
import Graphics.GLUtil.JuicyTextures (readTexture)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW as GLFW (MouseButton(..), MouseButtonState(..))

import Linear (V2(..), V3(..), M44, (!*!), perspective, translation, identity) -- Quaternion

import System.FilePath  (splitExtension, (</>))
import System.IO        (hFlush, stdout)

import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_, forM, liftM, unless, void, (<=<))
import Control.Applicative (liftA2)
import Control.Lens
import Control.Concurrent
-- import Control.Exception

-- import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable (Storable)
-- import qualified Data.Vector.Storable as V


import qualified Data.Text as T
import           Data.Maybe    (catMaybes)
import           Data.Either   (rights, lefts)
import qualified Data.Map as M

import Text.Printf (printf)
import Text.Read   (readMaybe)

import Data.IORef
import qualified Data.Set as Set

-- TODO: These functions (untilM, clamped, perhaps) should be imported from some sensibly named utility module instead
import Interactive.Console (chooseFilesFromDirectory, untilM, clamped, perhaps)

import           Graphics.Michelangelo.Texture            --
import           Graphics.Michelangelo.Transformations    --
import qualified Graphics.Michelangelo.Mesh    as Mesh    --
import qualified Graphics.Michelangelo.Lenses  as L
import qualified Graphics.Michelangelo.Shaders as Shaders --
import           Graphics.Michelangelo.Types (UniformValue(..), Mesh(..), Attribute(..)) --

import Cartesian.Core (centre, x, y, z, size, corner, width, height, depth)

import Leibniz.Constants    (π)
import Leibniz.Trigonometry (torad)

import Graphics.WaveFront.Types  as WF
import Graphics.WaveFront.Lenses as L
import Graphics.WaveFront.Load   as Load
import Graphics.WaveFront.Model  as WF



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Scene = Scene { fMeshes :: [Mesh Double Int] } -- deriving (Show)


-- |
data AppState = AppState {
  fRotation   :: V2 Double,
  fMouse      :: Maybe (V2 Double),
  fClientsize :: V2 Int,
  fFrame      :: Int,
  fScene      :: Scene
} -- deriving (Show)

makeLensesWith abbreviatedFields ''AppState
makeLensesWith abbreviatedFields ''Scene



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
grid :: (Integral n) => n -> n -> [(n, n)]
grid cols rows = [ (col, row) | col <- [1..cols], row <- [1..rows]]


-- |
gridM :: (Integral n, Monad m) => n -> n -> (n -> n -> m a) -> m [a]
gridM cols rows f = forM (grid cols rows) (uncurry f)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
initOpenGL :: IO ()
initOpenGL = do
  showOpenGLInformation
  -- depthTest $= Enabled
  depthFunc $= Just Lequal
  blend     $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)


-- |
-- Shader setup
-- TODO: Load all shaders from a given path (return Map) (?)
-- TODO: More detailed error message (?)
loadPrograms :: IO (Either [String] [Program])
loadPrograms = runEitherT $ mapM (\(vs, ps) -> EitherT $ Shaders.loadShaderProgram (root </> vs) (root </> ps)) shaderPaths
  where
    root        = "C:/Users/Jonatan/Desktop/Haskell/modules/Michelangelo/lib/Graphics/Michelangelo/shaders"
    shaderPaths = [("shader-vertex.glsl",          "shader-pixel.glsl"),
                   ("shader-textured-vertex.glsl", "shader-textured-pixel.glsl")]


-- |
render :: IORef AppState -> GLFW.WindowRefreshCallback
render stateref window = do
  --
  app <- readIORef stateref

  modifyIORef stateref $ frame +~ 1

  let (V2 rx ry) = float <$> app^.rotation
      (V2 cx cy) = float <$> app^.clientsize

  viewport   $= (Position 0 0, Size (floor $ app^.rotation.x) (floor $ app^.rotation.x))
  clearColor $= Color4 (0.2) (0.72) (0.23) (1.0)
  clear [ColorBuffer, DepthBuffer]

  let (cols, rows) = (5,5) :: (Int, Int)

  gridM cols rows $ \wxi wyi -> do
    let mesh'      = (app^.scene.meshes) !! 0 :: Mesh Double Int -- .ix 0 
        (V2 wx wy) = float <$> V2 wxi wyi
        bounds'    = mesh'^.L.bounds :: BoundingBox (V3 Double)
        size'@(V3 dx dy _) = bounds'^.size
        centre' = (bounds'^.corner) + ((*0.5) <$> size') -- bounds'^.centre

        motion = liftA2 (*) (V3 1.2 1.2 0.0) $ V3 (dx * (wx-1)) (dy*(wy-1) + 6*ry/cy) (6*rx/cx)

        modelview = identity & (translation .~ (motion - centre'))
        -- setModelview mesh = mesh { Mesh.uniforms=M.update (\(loc, _) -> Just (loc, UMatrix44 modelview)) "uMVMatrix" (Mesh.uniforms mesh) }
    forM_ (app^.scene.meshes) (Mesh.render . (L.uniforms.at "uMVMatrix"._Just._2 .~ UMatrix44 modelview))

  -- Text
  -- helloworld
  GLFW.swapBuffers window
  throwError

  where
    float :: (Real r, Fractional f) => r -> f -- TODO: Whuuuuuuut (monomorphism restriction)?
    float = realToFrac

    cint :: (Num n,  Integral i) => i -> n -- TODO: Ditto (?)
    cint = fromIntegral

--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Deal with missing values properly
-- TODO: Indexing should be defined in an API function

-- |
-- TODO: Factor out the buffer-building logic
fromIndices :: V.Vector (v Double) -> (VertexIndices Int64 ->  Int64) -> V.Vector (WF.Face Double T.Text Int64 V.Vector) -> V.Vector (Maybe (v Double))
fromIndices data' choose faces' = V.concatMap (fromFaceIndices data' choose) faces'


-- |
fromFaceIndices :: V.Vector (v Double) -> (VertexIndices Int64 ->  Int64) -> WF.Face Double T.Text Int64 V.Vector -> V.Vector (Maybe (v Double))
fromFaceIndices data' choose face' = V.map ((data' !?) . fromIntegral . choose) . (^.L.indices) $ face'


-- |
diffuseColours :: V.Vector (WF.Face f s i V.Vector) -> V.Vector (Colour f)
diffuseColours faces' = V.concatMap (\f -> V.replicate (V.length $ f^.L.indices) (f^.L.material.L.diffuse)) faces'


-- |
attr :: (Foldable t, Foldable v, Storable f) => GL.Program -> (String, t (v f)) -> EitherT String IO (String, Attribute Int)
attr theprogram = EitherT . uncurry (Mesh.newAttribute theprogram)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use index buffer (?)
-- TODO: Texture support
-- TODO: More flexible treatment of 'programs'
-- TODO: Unify the different mesh types (eg. use plain white texture)
createMesh :: (WF.SimpleModel -> Program) -> WF.SimpleModel -> IO (Either String (Mesh Double Int))
createMesh chooseProgram model = do

  putStrLn "Creating mesh from model"

  let bounds'               = WF.bounds model
  let centre'@(V3 cx cy cz) = (bounds'^.corner) + ((*0.5) <$> (bounds'^.size))

  printf "Width=%.02f, Height=%.02f, Depth=%.02f, Centre=(%.02f, %.02f, %.02f)\n" (bounds'^.width) (bounds'^.height) (bounds'^.depth) cx cy cz

  if WF.hasTextures model
    then createTexturedMesh (chooseProgram model) (model) (centre') --
    else createPaintedMesh  (chooseProgram model) (model) (centre') --


-- |
-- TODO: Wrap in Maybe (eg. for missing textures) (?)
-- TODO: Don't hard-code texture path
-- TODO: Default to painted when the textures fail to load (?)
createTexturedMesh :: Program -> WF.SimpleModel -> V3 Double -> IO (Either String (Mesh Double Int))
createTexturedMesh program' model centre' = runEitherT $ do
  attributes' <- sequence [attr program' ("aVertexPosition", vs),
                           attr program' ("aVertexColor",    cs),
                           attr program' ("aTexCoord",       ts)]
  
  -- TODO: This could fail
  (tex:tures) <- mapM (EitherT . readTexture . ("C:\\Users\\Jonatan\\Desktop\\3D\\models\\textures" </>) . T.unpack) (Set.toList $ WF.textures model)

  (locs, uniforms') <- lift $ do
    print (tex:tures)
    -- Uniforms
    -- TODO: Use activeTexture (?)
    locs      <- get $ uniformLocation program' "uSampler"
    uniforms' <- (at "uSampler" .~ Just (locs, UInt 0)) <$> defaultMatrixUniforms program'
    printError
    printf "Sampler location: %s\n" (show locs)
    return (locs, uniforms')

  -- TODO: Initialise properly
  return Mesh { fAttributes = M.fromList attributes',
                fPrimitive  = Triangles,
                fTexture    = Just tex,
                fShader     = program',
                fUniforms   = uniforms',
                fPrepare    = Just (\mesh -> GL.currentProgram $= Just (mesh^.L.shader)),
                fCentre     = centre',
                fBounds     = WF.bounds model,
                fSize       = (length $ vs) `div` 3 }
  where
    (Just vs) = sequence $ fromIndices (model^.vertices)  (^.ivertex)         (model^.faces)
    -- TODO: Deal with missing values properly (w.r.t texcoords and normals)
    (Just ts) = sequence $ fromIndices (model^.texcoords) (^.itexcoord.non 0) (model^.faces)
    cs = diffuseColours (model^.faces)


-- |
-- TODO: Use type synomyms
createPaintedMesh :: Program -> WF.SimpleModel -> V3 Double -> IO (Either String (Mesh Double Int))
createPaintedMesh program' model centre' = runEitherT $ do

  lift $ putStrLn "Creating painted mesh"

  -- Attributes
  -- TODO: Factor out buffer creation to Michelangelo (✓)
  attributes' <- sequence [attr program' ("aVertexPosition", vs),
                           attr program' ("aVertexColor",    cs)]
  
  -- Uniforms
  uniforms' <- lift $ defaultMatrixUniforms program'

  -- TODO: Initialise properly
  return $ Mesh { fAttributes = M.fromList attributes',
                  fPrimitive  = Triangles,
                  fTexture    = Nothing,
                  fShader     = program',
                  fUniforms   = uniforms',
                  fPrepare    = Just (\mesh -> GL.currentProgram $= Just (mesh^.L.shader)),
                  fCentre     = centre',
                  fBounds     = WF.bounds model,
                  fSize       = V.length vs }
  where
    (Just vs) = sequence $ fromIndices (model^.vertices) (^.ivertex) (model^.faces)
    cs = diffuseColours (model^.faces)


-- |
-- TODO: This is quite fragile at the moment
defaultMatrixUniforms :: Program -> IO (M.Map String (UniformLocation, UniformValue Double Int))
defaultMatrixUniforms program' = do
  [locmv, locmp] <- mapM (get . uniformLocation program') ["uMVMatrix", "uPMatrix"]

  -- TODO: Experiment with inverse perspective
  let modelview  = (rotateY (0.0 :: Double) !*! identity) & (translation .~ (V3 0 0 0))
      projection = (perspective
                     (torad 40.0) -- FOV (y direction, in radians)
                     1.0          -- Aspect ratio
                     1.0          -- Near plane
                     80.0)        -- Far plane

  return $ M.fromList [("uMVMatrix", (locmv, UMatrix44 modelview)), ("uPMatrix", (locmp, UMatrix44 projection))]


-- Events -----------------------------------------------------------------------------------------

-- |
-- animate fps = do
  -- postRedisplay Nothing
  -- addTimerCallback (div 1000 fps) (animate fps)


-- |
onmousedrag :: IORef AppState -> GLFW.CursorPosCallback
onmousedrag stateref _ mx my = do
  -- TODO: Refactor
  modifyIORef stateref $ \ state -> case state of
    AppState { fRotation=(V2 rx ry), fMouse=Just (V2 mx' my') } -> state { fRotation=V2 (rx+mx-mx') (ry+my-my'), fMouse=Just (V2 mx my) }
    _                                                           -> state


--  |
-- TODO: Rename (?)
-- Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
onmousepress :: IORef AppState -> GLFW.MouseButtonCallback
onmousepress stateref _      MouseButton'1 MouseButtonState'Released _ = modifyIORef stateref $ releasemouse
onmousepress stateref window MouseButton'1 MouseButtonState'Pressed  _ = GLFW.getCursorPos window >>= modifyIORef stateref . movemouse . uncurry V2
onmousepress _        _      _             _                         _ = return ()


-- |
movemouse mouse state = state { fMouse=Just mouse }


-- |
releasemouse state = state { fMouse=Nothing }


-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize stateref _ cx cy = do
  modifyIORef stateref $ \ state -> state { fClientsize=V2 cx cy }     --
  -- viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --
  putStrLn $ unwords ["Window size is", show cx, "/", show cy]


--------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: GLFW.Window -> IORef AppState -> IO ()
mainloop window stateref = do
  -- renderSimple window
  render stateref window
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose window
  unless closing $ mainloop window stateref

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
showOpenGLInformation :: IO ()
showOpenGLInformation = do
  printf "\n==== Version information =================================================\n"
  get vendor    >>= printf "Vendor:   %s\n"
  get renderer  >>= printf "Renderer: %s\n"
  get glVersion >>= printf "Version:  %s\n"
  get shadingLanguageVersion >>= printf "GLSL Version: %s\n"
  printf "==========================================================================\n\n"

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Move to module
listOBJFiles :: String -> IO (Either String [String])
listOBJFiles path = chooseFilesFromDirectory path (==".obj")


-- |
-- TODO: Console cursor (?)
chooseModelsFrom :: String -> IO (Either String String)
chooseModelsFrom path = runEitherT $ do
  paths <- EitherT $ listOBJFiles path
  choice <- lift $ do
    putStrLn "Which model would you like to load?"
    mapM option $ zip ([1..] :: [Int]) paths
    untilM (valid paths) (const $ prompt "That doesn't work. Try again: ") (prompt "Choose one: ")
  EitherT . return $ case choice of
    Just index -> Right $ paths !! (index-1) --
    Nothing    -> Left  $ "Invalid choice"   -- This should never happen, throw error instead (?)
  where
    valid paths = return . maybe False (clamped 0 (length paths) . (subtract 1)) --
    prompt q    = putStr q >> hFlush stdout >> (liftM readMaybe) getLine         -- Ask for input (flush is sometimes required when q doesn't end in a newline)
    possibly f x g   = either f g x                                              -- Do-block at the end instead of in the middle
    option (n, path) = printf "  [%d] %s\n" n path                               -- Prints a model option



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------


-- |
initial :: AppState
initial = AppState {
            fRotation   = V2 0 0,
            fMouse      = Nothing,
            fClientsize = V2 720 480,
            fFrame      = 1, -- TODO: Should this be 0 (?)
            fScene      = Scene { fMeshes = [] } }


-- |
fromBool :: Bool -> err -> a -> Either err a
fromBool True  _   a = Right a
fromBool False err _ = Left err


-- | Takes an action that signals success with a Bool, and turns it into an EitherT value
--   with an error message.
succeeded :: IO Bool -> String -> EitherT String IO ()
succeeded action message = do
  success <- lift action
  EitherT . return $ fromBool success message ()


-- |
openGLMain :: IO ()
openGLMain = (\outcome -> outcome >>= print) . runEitherT $ do
  --
  modelname <- lift $ do
    let path = "C:/Users/Jonatan/Desktop/3D/models/"
    modelpath <- chooseModelsFrom path 
    either (\_ -> putStrLn "Something went wrong. Using default model." >> return (path </> "king.obj")) return modelpath

  lift $ putStrLn "Loading model..."
  model <- EitherT $ Load.model modelname
  lift $ printf "Finished loading model '%s' with %d faces and %d vertices.\n" modelname (V.length $ model^.faces) (V.length $ model^.vertices)
  
  --
  stateref   <- lift $ newIORef initial
  (V2 cx cy) <- lift $ (^.clientsize) <$> readIORef stateref -- Yes, I know this is stupid.
  
  succeeded GLFW.init "Failed to initialise GLFW"
  -- mmonitor <- GLFW.getPrimaryMonitor
  -- GLFW.defaultWindowHints -- TODO: Before or after window creation (?)
  lift $ (GLFW.windowHint $ GLFW.WindowHint'Samples 4)
  window <- EitherT $ maybe (Left "Failed to create window") (Right) <$> (GLFW.createWindow cx cy "WaveFront OBJ Sample (2015)" Nothing Nothing)
  
  -- initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling]
  lift $ do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does
    
    -- GLFW.setWindowRefreshCallback window $ Just (render stateref buffers)
    GLFW.setMouseButtonCallback window $ Just (onmousepress   stateref)
    GLFW.setCursorPosCallback   window $ Just (onmousedrag    stateref)
    GLFW.setWindowSizeCallback  window $ Just (onwindowresize stateref)
    -- GLFW.setDropCallback      window $ Nothing
    -- GLFW.setCharCallback
    -- GLFW.setKeyCallback
    -- GLFW.setErrorCallback
    -- GLFW.addTimerCallback (div 100 30) (animate 30)
    initOpenGL --

  [painted, textured] <- EitherT $ either (Left . concat) (Right . id) <$> loadPrograms -- TODO: Don't make assumptions about the order of shaders

  meshes' <- mapM (EitherT . createMesh (\ _ -> if WF.hasTextures model then textured else painted)) [model]

  lift $ do
    modifyIORef stateref (scene.meshes .~ meshes')
    mainloop window stateref
    GLFW.destroyWindow window
    GLFW.terminate
    putStrLn "Finished"
    mapM_ putStrLn ["Finished painting.",
                     "Washing brushes...",
                     "Mounting canvas...",
                     "Disassembling easel...",
                     "Done. Good bye!"]
  return $ (Right "Everything went according to plan" :: Either String String)

-- |
main :: IO ()
main = do
  openGLMain
