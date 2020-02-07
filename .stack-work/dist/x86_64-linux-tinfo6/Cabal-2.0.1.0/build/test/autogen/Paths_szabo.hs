{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_szabo (
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

bindir     = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/bin"
libdir     = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/lib/x86_64-linux-ghc-8.2.2/szabo-0.1.0.0-1EXxaznSfaC58JVt6mC6iH-test"
dynlibdir  = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/share/x86_64-linux-ghc-8.2.2/szabo-0.1.0.0"
libexecdir = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/libexec/x86_64-linux-ghc-8.2.2/szabo-0.1.0.0"
sysconfdir = "/home/adam/Dropbox/haskell/linkhomology-trisections/.stack-work/install/x86_64-linux-tinfo6/2d164d0e7d62b9ee0c57cc113dfc8d3d12b349debd0669fac5a4d18ff54a2c0e/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "szabo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "szabo_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "szabo_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "szabo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "szabo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "szabo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
