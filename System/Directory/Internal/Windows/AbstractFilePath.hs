{-# LANGUAGE CPP #-}

module System.Directory.Internal.Windows.AbstractFilePath where


#if defined(mingw32_HOST_OS)

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

#include "HsBaseConfig.h"

#define FILEPATH AbstractFilePath
#define STRING OsString

import Data.String (fromString)
import qualified System.AbstractFilePath.Data.ByteString.Short.Word16 as L
import qualified System.Win32.WindowsString.Types as Win32
import qualified System.Win32.WindowsString.Shell as Win32
import qualified System.Win32.WindowsString.File as Win32
import qualified System.Win32.WindowsString.Info as Win32
import System.AbstractFilePath.Types
import System.OsString.Internal.Types
import System.Directory.Internal.WindowsFFI.AbstractFilePath
import System.Directory.Internal.WindowsFFI.Common
import System.Directory.Internal.Common.AbstractFilePath
import System.Directory.Internal.Config.AbstractFilePath (exeExtension)
import System.AbstractFilePath
  ( (</>)
  , isPathSeparator
  , isRelative
  , pathSeparator
  , splitDirectories
  , takeExtension
  )
import Foreign.Ptr (castPtr)

#include "Template.hs"

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif


lookupEnv :: AbstractFilePath -> IO (Maybe AbstractFilePath)
lookupEnv (OsString fp) = fmap OsString <$> lookupEnv' fp

lookupEnv' :: WindowsString -> IO (Maybe WindowsString)
lookupEnv' (WS name) = L.useAsCWString name $ \s -> try_size (castPtr s) 256
  where
    try_size s size = allocaArray (fromIntegral size) $ \p_value -> do
      res <- c_GetEnvironmentVariable s p_value size
      case res of
        0 -> return Nothing
        _ | res > size -> try_size s res -- Rare: size increased between calls to GetEnvironmentVariable
          | otherwise  -> L.packCWString (castPtr p_value) >>= return . Just . WS

foreign import WINDOWS_CCONV unsafe "windows.h GetEnvironmentVariableW"
  c_GetEnvironmentVariable :: Win32.LPWSTR -> Win32.LPWSTR -> Win32.DWORD -> IO Win32.DWORD

fromPlatformPath :: WindowsFilePath -> AbstractFilePath
fromPlatformPath = OsString

toPlatformPath :: AbstractFilePath -> WindowsFilePath
toPlatformPath (OsString fp) = fp

peekCWLen :: (Ptr CWchar, Int) -> IO AbstractFilePath
peekCWLen (ptr, i) = fmap (OsString . WS) . L.packCWStringLen $ (castPtr ptr, i)

#endif
