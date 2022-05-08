{-# LANGUAGE CPP #-}

module System.Directory.Internal.Posix.AbstractFilePath where

#if !defined(mingw32_HOST_OS)
import System.Directory.Internal.PosixFFI
import qualified System.AbstractFilePath.Data.ByteString.Short as L
import System.Directory.Internal.Common.AbstractFilePath

import System.AbstractFilePath
import System.OsString.Internal.Types
import System.AbstractFilePath.Posix (fromPlatformStringIO)
import System.Directory.Internal.Config.AbstractFilePath (exeExtension)

import qualified System.Posix.Env.PosixString as PS
import qualified System.Posix.Directory.PosixFilePath as Posix
import qualified System.Posix.Files.PosixString as Posix

import System.IO.Error (doesNotExistErrorType)

#define FILEPATH AbstractFilePath
#define STRING OsString
#include "Template.hs"

lookupEnv :: OsString -> IO (Maybe OsString)
lookupEnv (OsString name@(PS _)) = fmap OsString <$> PS.getEnv name

getEnv :: OsString -> IO OsString
getEnv (OsString name@(PS _)) = do
  env <- PS.getEnv name
  pn <- fromPlatformStringIO name
  case env of
    Nothing -> throwIO (mkIOError doesNotExistErrorType ("Env var '" <> pn <> "' could not be found!") Nothing Nothing)
    Just value -> pure (OsString value)


canonicalizePathWith :: ((AbstractFilePath -> IO AbstractFilePath) -> AbstractFilePath -> IO AbstractFilePath)
                     -> AbstractFilePath
                     -> IO AbstractFilePath
canonicalizePathWith attemptRealpath path = do
  let realpath (OsString (PS sb)) = L.useAsCString sb (\cstr -> withRealpath cstr (fmap (OsString . PS) . L.packCString))
  attemptRealpath realpath path
#endif
