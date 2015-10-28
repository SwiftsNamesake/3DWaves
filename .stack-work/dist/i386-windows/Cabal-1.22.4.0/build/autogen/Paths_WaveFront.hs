module Paths_WaveFront (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\WaveFront\\.stack-work\\install\\i386-windows\\lts-3.5\\7.10.2\\bin"
libdir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\WaveFront\\.stack-work\\install\\i386-windows\\lts-3.5\\7.10.2\\lib\\i386-windows-ghc-7.10.2\\WaveFront-0.1.0.0-Bmgqj22QsG4JDYPIKvoJPR"
datadir    = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\WaveFront\\.stack-work\\install\\i386-windows\\lts-3.5\\7.10.2\\share\\i386-windows-ghc-7.10.2\\WaveFront-0.1.0.0"
libexecdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\WaveFront\\.stack-work\\install\\i386-windows\\lts-3.5\\7.10.2\\libexec"
sysconfdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\WaveFront\\.stack-work\\install\\i386-windows\\lts-3.5\\7.10.2\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "WaveFront_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "WaveFront_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "WaveFront_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "WaveFront_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "WaveFront_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
