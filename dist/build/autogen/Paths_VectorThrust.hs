module Paths_VectorThrust (
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

bindir     = "C:\\Users\\Tschaka\\Desktop\\GdI Projekt SS16 - FRP Spiele Programmierung\\VectorThrust\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\Tschaka\\Desktop\\GdI Projekt SS16 - FRP Spiele Programmierung\\VectorThrust\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\VectorThrust-0.1.0.0-Gpry4AJBVOKLByZns7HYRH"
datadir    = "C:\\Users\\Tschaka\\Desktop\\GdI Projekt SS16 - FRP Spiele Programmierung\\VectorThrust\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\VectorThrust-0.1.0.0"
libexecdir = "C:\\Users\\Tschaka\\Desktop\\GdI Projekt SS16 - FRP Spiele Programmierung\\VectorThrust\\.cabal-sandbox\\VectorThrust-0.1.0.0-Gpry4AJBVOKLByZns7HYRH"
sysconfdir = "C:\\Users\\Tschaka\\Desktop\\GdI Projekt SS16 - FRP Spiele Programmierung\\VectorThrust\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "VectorThrust_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "VectorThrust_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "VectorThrust_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "VectorThrust_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "VectorThrust_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
