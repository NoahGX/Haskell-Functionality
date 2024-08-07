{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_assign2_haskell (
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

bindir     = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/bin"
libdir     = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/lib/x86_64-osx-ghc-8.10.7/assign2-haskell-0.1.0.0-FQeGyz6AWLf4VH14RgrkgI-test"
dynlibdir  = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/share/x86_64-osx-ghc-8.10.7/assign2-haskell-0.1.0.0"
libexecdir = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/libexec/x86_64-osx-ghc-8.10.7/assign2-haskell-0.1.0.0"
sysconfdir = "/Users/noahgarcia/Haskell/Assign2/.stack-work/install/x86_64-osx/a9028f55b2d1693e0498c4d3c9138dddcd7769ae6d2debece53ef87b31f38475/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assign2_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assign2_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "assign2_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "assign2_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assign2_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assign2_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
