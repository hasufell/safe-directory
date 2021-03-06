{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Stability: unstable
-- Portability: portable
--
-- Internal modules are always subject to change from version to version.

module System.Directory.Internal.Prelude
  ( module Prelude
#if !MIN_VERSION_base(4, 6, 0)
  , forkFinally
  , lookupEnv
#endif
#if !MIN_VERSION_base(4, 8, 0)
  , module Control.Applicative
  , module Data.Functor
#endif
  , module Control.Arrow
  , module Control.Concurrent
  , module Control.Exception
  , module Control.Monad
  , module Data.Bits
  , module Data.Char
  , module Data.Foldable
  , module Data.Function
  , module Data.Maybe
  , module Data.Monoid
  , module Data.IORef
  , module Data.Traversable
  , module Foreign
  , module Foreign.C
  , module GHC.IO.Encoding
  , module GHC.IO.Exception
  , module System.Environment
  , module System.Exit
  , module System.IO
  , module System.IO.Error
  , module System.Posix.Internals
  , module System.Posix.Types
  , module System.Timeout
  , Void
  ) where
#if MIN_VERSION_base(4, 6, 0)
import System.Environment (lookupEnv)
#else
import Prelude hiding (catch)
#endif
#if MIN_VERSION_base(4, 8, 0)
import Data.Void (Void)
#else
import Control.Applicative (Applicative, (<*>), (*>), pure)
import Data.Functor ((<$>), (<$))
#endif
import Control.Arrow (second)
import Control.Concurrent
  ( forkIO
  , killThread
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
#if MIN_VERSION_base(4, 6, 0)
  , forkFinally
#else
  , ThreadId
#endif
  )
import Control.Exception
  ( SomeException
  , bracket
  , bracket_
  , catch
  , finally
  , mask
  , onException
  , throwIO
  , try
  )
import Control.Monad ((>=>), (<=<), unless, when, replicateM, replicateM_)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (isAlpha, isAscii, toLower, toUpper)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>), mconcat, mempty)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Traversable (for)
import Foreign
  ( Ptr
  , Storable
    ( alignment
    , peek
    , peekByteOff
    , peekElemOff
    , poke
    , pokeByteOff
    , pokeElemOff
    , sizeOf
    )
  , alloca
  , allocaArray
  , allocaBytes
  , allocaBytesAligned
  , maybeWith
  , nullPtr
  , plusPtr
  , with
  , withArray
  )
import Foreign.C
  ( CInt(..)
  , CLong(..)
  , CString
  , CTime(..)
  , CUChar(..)
  , CULong(..)
  , CUShort(..)
  , CWString
  , CWchar(..)
  , peekCString
  , peekCWStringLen
  , throwErrnoIfMinus1Retry_
  , throwErrnoIfMinus1_
  , throwErrnoIfNull
  , throwErrnoPathIfMinus1_
  , withCString
  , withCWString
  )
import GHC.IO.Exception
  ( IOErrorType
    ( InappropriateType
    , InvalidArgument
    , OtherError
    , UnsupportedOperation
    )
  )
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO
  ( Handle
  , IOMode(ReadMode, WriteMode)
  , hClose
  , hFlush
  , hGetBuf
  , hPutBuf
  , hPutStr
  , hPutStrLn
  , openBinaryTempFile
  , stderr
  , stdout
  , withBinaryFile
  )
import System.IO.Error
  ( IOError
  , catchIOError
  , illegalOperationErrorType
  , ioeGetErrorString
  , ioeGetErrorType
  , ioeGetLocation
  , ioeSetErrorString
  , ioeSetFileName
  , ioeSetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isIllegalOperation
  , isPermissionError
  , mkIOError
  , modifyIOError
  , permissionErrorType
  , tryIOError
  , userError
  )
import System.Posix.Internals (withFilePath)
import System.Posix.Types (EpochTime)
import System.Timeout (timeout)

#if !MIN_VERSION_base(4, 6, 0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = do
  env <- tryIOError (getEnv name)
  case env of
    Left err | isDoesNotExistError err -> pure Nothing
             | otherwise               -> throwIO err
    Right value -> pure (Just value)

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif

#if !MIN_VERSION_base(4, 8, 0)
data Void = Void

_unusedVoid :: Void
_unusedVoid = Void
#endif
