{-# LANGUAGE CPP #-}
module System.Directory.Internal.Windows where

#if defined(mingw32_HOST_OS)

#define FILEPATH FilePath
#define STRING String

import Data.String (fromString)
import qualified System.Win32.Types as Win32
import qualified System.Win32.Shell as Win32
import qualified System.Win32.File as Win32
import qualified System.Win32.Info as Win32
import System.Directory.Internal.WindowsFFI
import System.Directory.Internal.WindowsFFI.Common
import System.Directory.Internal.Common
import System.Directory.Internal.Config (exeExtension)
import System.FilePath
  ( (</>)
  , isPathSeparator
  , isRelative
  , pathSeparator
  , splitDirectories
  , takeExtension
  )

#include "Windows/Template.hs"

fromPlatformPath :: FilePath -> FilePath
fromPlatformPath = id

toPlatformPath :: FilePath -> FilePath
toPlatformPath = id

peekCWLen :: (Ptr CWchar, Int) -> IO String
peekCWLen = peekCWStringLen


#endif
