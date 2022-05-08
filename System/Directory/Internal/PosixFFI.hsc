{-# LANGUAGE CPP #-}

module System.Directory.Internal.PosixFFI where

#include <HsDirectoryConfig.h>
#if !defined(mingw32_HOST_OS)
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude


-- we use the 'free' from the standard library here since it's not entirely
-- clear whether Haskell's 'free' corresponds to the same one
foreign import ccall unsafe "free" c_free :: Ptr a -> IO ()

#if !defined(HAVE_REALPATH)

c_realpath :: CString -> CString -> IO CString
c_realpath _ _ = throwIO (mkIOError UnsupportedOperation "platform does not support realpath" Nothing Nothing)

#else

foreign import ccall "realpath" c_realpath :: CString -> CString -> IO CString

#endif
#endif
