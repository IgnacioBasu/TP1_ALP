{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\bin"
libdir     = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\lib\\x86_64-windows-ghc-8.8.4\\TP2-0.1.0.0-KJ26XcQb6zja8ZUcR6HAi-TP2-exe"
dynlibdir  = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\share\\x86_64-windows-ghc-8.8.4\\TP2-0.1.0.0"
libexecdir = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\libexec\\x86_64-windows-ghc-8.8.4\\TP2-0.1.0.0"
sysconfdir = "C:\\Users\\Lautaro\\Documents\\GitHub\\TP1_ALP\\TP2\\.stack-work\\install\\d03249e7\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
