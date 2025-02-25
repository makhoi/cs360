{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_examples (
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
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/bin"
libdir     = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/lib/aarch64-osx-ghc-9.4.8/examples-0.1-27ElsXIH93e6fsDJHxumQC"
dynlibdir  = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/share/aarch64-osx-ghc-9.4.8/examples-0.1"
libexecdir = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/libexec/aarch64-osx-ghc-9.4.8/examples-0.1"
sysconfdir = "/Users/khoima/Desktop/Winter2425/CS360/lectures/lecture11-programs-as-data/.stack-work/install/aarch64-osx/a81541c53fa2b2b3eabcb72628902d84357e6cf11e986180ff76b86f045916a3/9.4.8/etc"

getBinDir     = catchIO (getEnv "examples_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "examples_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "examples_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "examples_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "examples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "examples_sysconfdir") (\_ -> return sysconfdir)




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
