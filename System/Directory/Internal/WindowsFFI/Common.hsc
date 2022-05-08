{-# LANGUAGE CPP #-}
module System.Directory.Internal.WindowsFFI.Common where
#include <HsDirectoryConfig.h>
#if defined(mingw32_HOST_OS)
##if defined(i386_HOST_ARCH)
## define WINAPI stdcall
##elif defined(x86_64_HOST_ARCH)
## define WINAPI ccall
##else
## error unknown architecture
##endif
#include <shlobj.h>
#include <windows.h>
#include <System/Directory/Internal/utility.h>
#include <System/Directory/Internal/windows_ext.h>
import Prelude ()
import System.Directory.Internal.Prelude
import GHC.Word (Word32)
import qualified System.Win32 as Win32


#ifdef HAVE_GETFINALPATHNAMEBYHANDLEW
foreign import WINAPI unsafe "windows.h GetFinalPathNameByHandleW"
  c_GetFinalPathNameByHandle
    :: Win32.HANDLE
    -> Ptr CWchar
    -> Win32.DWORD
    -> Win32.DWORD
    -> IO Win32.DWORD

#endif


foreign import WINAPI unsafe "windows.h DeviceIoControl"
  c_DeviceIoControl
    :: Win32.HANDLE
    -> Win32.DWORD
    -> Ptr a
    -> Win32.DWORD
    -> Ptr b
    -> Win32.DWORD
    -> Ptr Win32.DWORD
    -> Ptr Void
    -> IO Win32.BOOL


#ifdef HAVE_CREATESYMBOLICLINKW
foreign import WINAPI unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink
    :: Ptr CWchar -> Ptr CWchar -> Win32.DWORD -> IO Win32.BYTE
#endif

win32_cSIDL_COMMON_APPDATA :: Win32.CSIDL
win32_cSIDL_COMMON_APPDATA = (#const CSIDL_COMMON_APPDATA)

win32_eRROR_INVALID_FUNCTION :: Win32.ErrCode
win32_eRROR_INVALID_FUNCTION = (#const ERROR_INVALID_FUNCTION)

win32_eRROR_INVALID_PARAMETER :: Win32.ErrCode
win32_eRROR_INVALID_PARAMETER = (#const ERROR_INVALID_PARAMETER)

win32_eRROR_PRIVILEGE_NOT_HELD :: Win32.ErrCode
win32_eRROR_PRIVILEGE_NOT_HELD = (#const ERROR_PRIVILEGE_NOT_HELD)

max_path_len :: Word32
max_path_len = (#const MAX_PATH) * (#size wchar_t)

#endif
