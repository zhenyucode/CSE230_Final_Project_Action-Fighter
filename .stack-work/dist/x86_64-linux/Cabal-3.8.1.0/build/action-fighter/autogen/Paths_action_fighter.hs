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
bindir     = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/bin"
libdir     = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/lib/x86_64-linux-ghc-9.4.8/action-fighter-0.1.0.0-JZp9ZDVb9XpGQJAHwYbKxH-action-fighter"
dynlibdir  = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/share/x86_64-linux-ghc-9.4.8/action-fighter-0.1.0.0"
libexecdir = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/libexec/x86_64-linux-ghc-9.4.8/action-fighter-0.1.0.0"
sysconfdir = "/home/CSE230_Final_Project_Action-Fighter/.stack-work/install/x86_64-linux/b6c7380c67bc3117eb93951e7867c36dbc6fe669e48b8bee267fb4ed4611da9e/9.4.8/etc"

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
