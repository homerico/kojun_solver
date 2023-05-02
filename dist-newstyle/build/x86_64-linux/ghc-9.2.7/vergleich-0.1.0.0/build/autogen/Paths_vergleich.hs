{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vergleich (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/pedrohdq/.cabal/bin"
libdir     = "/home/pedrohdq/.cabal/lib/x86_64-linux-ghc-9.2.7/vergleich-0.1.0.0-inplace"
dynlibdir  = "/home/pedrohdq/.cabal/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/pedrohdq/.cabal/share/x86_64-linux-ghc-9.2.7/vergleich-0.1.0.0"
libexecdir = "/home/pedrohdq/.cabal/libexec/x86_64-linux-ghc-9.2.7/vergleich-0.1.0.0"
sysconfdir = "/home/pedrohdq/.cabal/etc"

getBinDir     = catchIO (getEnv "vergleich_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vergleich_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vergleich_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vergleich_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vergleich_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vergleich_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
