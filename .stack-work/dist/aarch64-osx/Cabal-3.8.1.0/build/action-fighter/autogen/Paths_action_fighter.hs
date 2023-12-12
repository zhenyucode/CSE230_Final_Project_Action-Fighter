{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_action_fighter (
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
bindir     = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/bin"
libdir     = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/lib/aarch64-osx-ghc-9.4.7/action-fighter-0.1.0.0-KcAYY08Cje2AenEwr69HpE-action-fighter"
dynlibdir  = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/share/aarch64-osx-ghc-9.4.7/action-fighter-0.1.0.0"
libexecdir = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/libexec/aarch64-osx-ghc-9.4.7/action-fighter-0.1.0.0"
sysconfdir = "/Users/zhenyulu/Desktop/Gitcode/CSE230_Final_Project_Action-Fighter/.stack-work/install/aarch64-osx/0f09181f9beaf17a649f9d2bc9e9bc96ea0155063ae8173fcb0b0905ea4480cf/9.4.7/etc"

getBinDir     = catchIO (getEnv "action_fighter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "action_fighter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "action_fighter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "action_fighter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "action_fighter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "action_fighter_sysconfdir") (\_ -> return sysconfdir)




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
