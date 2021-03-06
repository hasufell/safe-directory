{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Stability: unstable
-- Portability: unportable
--
-- Internal modules are always subject to change from version to version.
-- The contents of this module are also platform-dependent, hence what is
-- shown in the Hackage documentation may differ from what is actually
-- available on your system.

#include <HsDirectoryConfig.h>

module System.Directory.Internal.OsPath
  ( module System.Directory.Internal.Common.OsPath

#if defined(mingw32_HOST_OS)
  , module System.Directory.Internal.Windows.OsPath
#else
  , module System.Directory.Internal.Posix.OsPath
#endif

  ) where

import System.Directory.Internal.Common.OsPath

#if defined(mingw32_HOST_OS)
import System.Directory.Internal.Windows.OsPath
#else
import System.Directory.Internal.Posix.OsPath
#endif
