{-# LANGUAGE CPP #-}

module System.Directory.Internal.Posix where

#if !defined(mingw32_HOST_OS)
import System.Directory.Internal.PosixFFI
import System.Directory.Internal.Common
import System.FilePath ((</>), isRelative, splitSearchPath)
import qualified GHC.Foreign as GHC
import System.Directory.Internal.Prelude (lookupEnv, getEnv)
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import System.Directory.Internal.Config (exeExtension)

#define FILEPATH FilePath
#define STRING String
#include "Posix/Template.hs"

canonicalizePathWith :: ((FilePath -> IO FilePath) -> FilePath -> IO FilePath)
                     -> FilePath
                     -> IO FilePath
canonicalizePathWith attemptRealpath path = do
  encoding <- getFileSystemEncoding
  let realpath path' =
        GHC.withCString encoding path' (`withRealpath` GHC.peekCString encoding)
  attemptRealpath realpath path

#endif
