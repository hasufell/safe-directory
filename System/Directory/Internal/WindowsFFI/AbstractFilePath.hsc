{-# LANGUAGE CPP #-}

module System.Directory.Internal.WindowsFFI.AbstractFilePath where

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

import System.OsString.Internal.Types
import System.AbstractFilePath.Data.ByteString.Short.Word16 (packCWStringLen)
import Prelude ()
import System.Directory.Internal.Prelude
import qualified System.Win32 as Win32

data Win32_REPARSE_DATA_BUFFER
  = Win32_MOUNT_POINT_REPARSE_DATA_BUFFER WindowsString WindowsString
    -- ^ substituteName printName
  | Win32_SYMLINK_REPARSE_DATA_BUFFER WindowsString WindowsString Bool
    -- ^ substituteName printName isRelative
  | Win32_GENERIC_REPARSE_DATA_BUFFER

win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE :: Win32.DWORD
win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE =
  (#const MAXIMUM_REPARSE_DATA_BUFFER_SIZE)

win32_iO_REPARSE_TAG_MOUNT_POINT, win32_iO_REPARSE_TAG_SYMLINK :: CULong
win32_iO_REPARSE_TAG_MOUNT_POINT = (#const IO_REPARSE_TAG_MOUNT_POINT)
win32_iO_REPARSE_TAG_SYMLINK = (#const IO_REPARSE_TAG_SYMLINK)

win32_sYMLINK_FLAG_RELATIVE :: CULong
win32_sYMLINK_FLAG_RELATIVE = 0x00000001

win32_alloca_REPARSE_DATA_BUFFER
  :: ((Ptr Win32_REPARSE_DATA_BUFFER, Int) -> IO a) -> IO a
win32_alloca_REPARSE_DATA_BUFFER action =
  allocaBytesAligned size align $ \ ptr ->
    action (ptr, size)
  where size = fromIntegral win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE
        -- workaround (hsc2hs for GHC < 8.0 don't support #{alignment ...})
        align = #{size char[alignof(HsDirectory_REPARSE_DATA_BUFFER)]}

win32_peek_REPARSE_DATA_BUFFER
  :: Ptr Win32_REPARSE_DATA_BUFFER -> IO Win32_REPARSE_DATA_BUFFER
win32_peek_REPARSE_DATA_BUFFER p = do
  tag <- #{peek HsDirectory_REPARSE_DATA_BUFFER, ReparseTag} p
  case () of
    _ | tag == win32_iO_REPARSE_TAG_MOUNT_POINT -> do
          let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.PathBuffer} p
          sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.SubstituteNameOffset} p
          sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.SubstituteNameLength} p
          sn <- peekName buf sni sns
          pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.PrintNameOffset} p
          pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.PrintNameLength} p
          pn <- peekName buf pni pns
          pure (Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn pn)
      | tag == win32_iO_REPARSE_TAG_SYMLINK -> do
          let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.PathBuffer} p
          sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.SubstituteNameOffset} p
          sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.SubstituteNameLength} p
          sn <- peekName buf sni sns
          pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.PrintNameOffset} p
          pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.PrintNameLength} p
          pn <- peekName buf pni pns
          flags <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.Flags} p
          pure (Win32_SYMLINK_REPARSE_DATA_BUFFER sn pn
                (flags .&. win32_sYMLINK_FLAG_RELATIVE /= 0))
      | otherwise -> pure Win32_GENERIC_REPARSE_DATA_BUFFER

peekName :: Ptr CWchar -> CUShort -> CUShort -> IO WindowsString
peekName buf offset size = WS <$>
  packCWStringLen ( buf `plusPtr` fromIntegral offset
                  , fromIntegral size `div` sizeOf (0 :: CWchar) )


#endif
