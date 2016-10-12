-- |
-- Module      : SampleApp - Main
-- Description : Executable which loads a model and displays it using OpenGL
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)

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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE NamedFieldPuns         #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Vector as V
import           Data.Vector (Vector, (!?))

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe    (listToMaybe, fromMaybe)
import           Data.Either   (rights, lefts)
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.IORef
import qualified Data.Set as S

import Text.Printf (printf)
import Text.Read   (readMaybe)

import Linear (V2(..), V3(..), M44, (!*!), perspective, translation, identity) -- Quaternion

import System.Console.ANSI
import System.FilePath  (splitExtension, takeBaseName, (</>))
import System.IO        (hFlush, stdout)

import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_, forM, liftM, unless, forever, void, (<=<))
import Control.Monad.Extra (skip)
import Control.Bool
import Control.Applicative (liftA2)
import Control.Lens
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Async

import GHC.Stack

import Foreign.C.Types
import Foreign.Storable (Storable)


import           Graphics.Rendering.OpenGL as GL hiding (perspective, ortho, rotate, translate, scale, texture)
import           Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import           Graphics.GLUtil hiding        (loadShaderProgram)
import           Graphics.GLUtil.JuicyTextures (readTexture)

import           Graphics.UI.GLFW (MouseButton(..), MouseButtonState(..))
import qualified Graphics.UI.GLFW as GLFW

import Reactive.Banana
import Reactive.Banana.Frameworks

import           Graphics.Michelangelo.Texture            --
import           Graphics.Michelangelo.Transformations    --
import qualified Graphics.Michelangelo.Mesh    as Mesh    --
import qualified Graphics.Michelangelo.Lenses  as L
import qualified Graphics.Michelangelo.Shaders as Shader --
import           Graphics.Michelangelo.Types (UniformValue(..), Mesh(..), Attribute(..)) --

import Cartesian.Core (centre, x, y, z, size, corner, width, height, depth)

import Leibniz.Constants    (π)
import Leibniz.Trigonometry (torad)

import Reactive.Banana.GLFW.Listeners

import Graphics.WaveFront.Types  as WF
import Graphics.WaveFront.Model  as WF
import Graphics.WaveFront.Lenses as L hiding (texture)
import Graphics.WaveFront.Load   as Load
import Graphics.WaveFront.Model  as WF



--------------------------------------------------------------------------------------------------------------------------------------------
-- The Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Scene = Scene { fMeshes :: [Mesh Double Int] } -- deriving (Show)


-- |
-- TODO: START USING QUATERNIONS ALREADY
data Camera f = Camera {
  fRotation    :: M44 Double,
  fPosition    :: M44 Double
}


-- | Pure data that is used to initialise the application state
data Config = Config {
  fClientsize :: V2 Int
}


-- |
data AppState = AppState {
  fCamera     :: Camera Double,
  fMouse      :: Maybe (V2 Double),
  fClientsize :: V2 Int,
  fFrame      :: Int,
  fScene      :: Scene,
  fWindow     :: GLFW.Window,
  fCommand    :: MVar String
} -- deriving (Show)


makeLensesWith abbreviatedFields ''AppState
makeLensesWith abbreviatedFields ''Camera
makeLensesWith abbreviatedFields ''Config
makeLensesWith abbreviatedFields ''Scene



--------------------------------------------------------------------------------------------------------------------------------------------
-- The Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Control flow ----------------------------------------------------------------------------------------------------------------------------

-- | Takes an action that signals success with a Bool, and turns it into an EitherT value
--   with an error message.
-- TODO | Rename
succeeded :: String -> IO Bool -> EitherT String IO ()
succeeded message action = do
  success <- lift action
  EitherT . return $ fromBool message () success


-- |
fromMaybe :: err -> Maybe a -> Either err a
fromMaybe err ma = maybe (Left err) (Right) ma


-- | 
fromBool :: err -> a -> Bool -> Either err a
fromBool _   a True  = Right a
fromBool err _ False = Left err

-- Graphics --------------------------------------------------------------------------------------------------------------------------------

-- |
prepareGraphics :: IO ()
prepareGraphics = do
  -- depthTest $= Enabled
  depthFunc $= Just Lequal
  blend     $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)


-- |
render :: AppState -> IO ()
render app = do
  viewport   $= (Position 0 0, Size cx cy)
  clearColor $= Color4 (0.2) (0.72) (0.23) (1.0)
  clear [ColorBuffer, DepthBuffer]


  GLFW.swapBuffers (app^.window)
  where
    (V2 cx cy) = fromIntegral <$> app^.clientsize


-- |
attr :: (Foldable t, Foldable v, Storable f, Real f) => GL.Program -> (String, t (v f)) -> EitherT String IO (String, Attribute Int)
attr program' = EitherT . uncurry (Shader.newAttribute program')


-- |
-- TODO: This needs a lot of work (cf. newAttribute for inspiration)
-- TODO: Factor out uniform logic to Michelangelo (cf. attributes)
defaultUniforms :: Program -> IO (M.Map String (UniformLocation, UniformValue Double Int))
defaultUniforms program' = do
  [locs, locmv, locmp] <- mapM (get . uniformLocation program') ["uSampler", "uMVMatrix", "uPMatrix"]
  printError
  printf "Sampler location: %s\n" (show locs)
  return $ M.fromList [("uSampler", (locs, UInt 0)), ("uMVMatrix", (locmv, UMatrix44 modelview)), ("uPMatrix", (locmp, UMatrix44 projection))]
  where
    -- TODO: Experiment with inverse perspective
    modelview  = (rotateY (0.0 :: Double) !*! identity) & (translation .~ (V3 0 0 0))
    projection = (perspective
                   (torad 40.0) -- FOV (y direction, in radians)
                   1.0          -- Aspect ratio
                   1.0          -- Near plane
                   80.0)        -- Far plane

-- Scenes ----------------------------------------------------------------------------------------------------------------------------------

-- |
createMesh :: Program -> (Model Double Text Int Vector) -> IO (Either String (Mesh Double Int))
createMesh program' model = runEitherT $ do
  attributes' <- sequence [attr program' ("aVertexPosition", vs),
                           attr program' ("aVertexColor",    cs),
                           attr program' ("aTexCoord",       ts)]
  
  -- TODO: This could fail
  texture'  <- fromMaybe white . listToMaybe $ mapM (EitherT . readTexture . texturePath . T.unpack) (S.toList $ WF.textures model)
  uniforms' <- lift (defaultUniforms program')
  
  lift $ do
    putStrLn $ "#VS: " ++ show (V.length vs)
    -- putStrLn $ "#CS: " ++ show (V.length cs)
    putStrLn $ "#TS: " ++ show (V.length ts)
    putStrLn $ "#F:  " ++ show (V.length $ model^.faces)

  -- TODO: Initialise properly
  return Mesh { fAttributes = M.fromList attributes',
                fPrimitive  = GL.Triangles,
                fTexture    = texture',
                fShader     = program',
                fUniforms   = uniforms',
                fPrepare    = Just prepareTextured,
                fCentre     = V3 0 0 0,
                fBounds     = WF.bounds model,
                fSize       = (length $ vs) }
  where
    (Just vs) = sequence $ WF.fromIndices (model^.vertices)  (^.ivertex) (model^.faces)
    -- TODO: Deal with missing values properly (w.r.t texcoords and normals)
    (Just ts) = sequence $ WF.fromIndices (model^.texcoords) (^.itexcoord.non 0) (model^.faces)
    -- cs = diffuseColours (model^.faces)
    texturePath = ((maybe "." id (model^.root) </> "textures/") </>)
    prepareTextured mesh = do
      GL.currentProgram $= Just (mesh^.L.shader)
      maybe (putStrLn "Mesh has no texture") (setTexture program') (mesh^.L.texture)

-- Interaction -----------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: GLFW.MouseButtonCallback
onmousepress _ _ _ _ = putStrLn "Mouse button was pressed"

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: IORef AppState -> IO ()
mainloop appref = do
  app <- readIORef appref
  maybe skip (runCommand app) <$> tryTakeMVar (app^.command)
  render app
  GLFW.pollEvents
  unlessM (GLFW.windowShouldClose (app^.window)) (mainloop appref)

-- Loading ---------------------------------------------------------------------------------------------------------------------------------

-- |
timeit :: IO a -> (Double -> String) -> (a -> Double -> String) -> IO a
timeit action begin done = do
  (Just before) <- GLFW.getTime
  putStrLn (begin before) >> hFlush stdout
  value <- action
  (Just after) <- GLFW.getTime
  putStrLn (done value (after - before)) >> hFlush stdout
  return value


-- |
loadWithName :: FilePath -> IO (String, Either String (Model Double Text Int Vector))
loadWithName fn = timeit load begin done
  where
    name       = takeBaseName fn
    load       = (name,) <$> Load.model fn
    begin _    = printf "Loading %s...\n" name
    done  m dt = printf "Done loading %s (it took %.02f seconds)\n" name dt


-- |
loadModels :: [FilePath] -> IO (M.Map String (Either String (Model Double Text Int Vector)))
loadModels fns = M.fromList <$> mapConcurrently (loadWithName) fns


-- |
loadMeshes :: GL.Program -> [FilePath] -> IO (M.Map String (Either String (Mesh Double Int)))
loadMeshes shader' fns = do
  models <- loadModels fns
  -- 
  -- Either String SimpleModel -> IO (Either String (Mesh Double Int))
  mapM (either (return . Left) (createMesh shader')) models

-- Scripting -------------------------------------------------------------------------------------------------------------------------------

-- |
feedCommand :: MVar String -> IO ()
feedCommand command' = getLine >>= putMVar command'


-- |
runCommand :: AppState -> String -> IO ()
runCommand app cmd = do
  return ()

-- FRP -------------------------------------------------------------------------------------------------------------------------------------

-- Data ------------------------------------------------------------------------------------------------------------------------------------

-- | Global configuration data.
config = Config {
  fClientsize = V2 720 480
}


-- | The initial application state
initial :: GLFW.Window -> MVar String -> Config -> AppState
initial window' command' config' = AppState {
                                     fCamera     = Camera { fRotation = identity, fPosition = identity },
                                     fMouse      = Nothing,
                                     fClientsize = config'^.clientsize,
                                     fFrame      = 1, -- TODO: Should this be 0 (?)
                                     fWindow     = window',
                                     fCommand    = command',
                                     fScene      = Scene { fMeshes = [] } }

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
createWindow :: V2 Int -> IO (Maybe GLFW.Window)
createWindow (V2 cx cy) = GLFW.createWindow cx cy "WaveFront OBJ Viewer (2016)" Nothing Nothing


-- |
windowFlags :: IO ()
windowFlags = GLFW.windowHint $ GLFW.WindowHint'Samples 4


-- | 
showOutcome :: IO (Either String ()) -> IO ()
showOutcome outcome = outcome >>= either print (const $ putStrLn "The program finished normally.")

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = showOutcome . runEitherT $ do
  succeeded "Failed to create window" GLFW.init
  lift windowFlags
  window' <- EitherT $ fromMaybe "Failed to create window" <$> createWindow (config^.clientsize)
  
  lift $ do
    GLFW.makeContextCurrent $ Just window' -- Not sure why this is needed or what it does
    unregister <- registerMouseButtonHandler window' onmousepress
    return ()
  
  lift prepareGraphics
  shader' <- EitherT (Shader.createProgram Shader.textured)
  meshes' <- lift $ loadMeshes shader' ["assets/models/hombre.obj", "assets/models/minecraft1.obj", "assets/models/king.obj", "assets/models/villa.obj"]
  let meshList = map snd . M.toList . (M.mapMaybe (either (const Nothing) Just)) $ meshes'
  lift $ do
    command' <- newEmptyMVar
    appref   <- newIORef (initial window' command' config & scene.meshes .~ meshList)
    cmd <- async (forever $ feedCommand command')
    mainloop appref
    cancel cmd
    GLFW.destroyWindow window'
    GLFW.terminate


-- |
-- reactive = start $ do
--   network <- compile networkDescription
--   actuate network