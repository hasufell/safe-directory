{-# LANGUAGE CPP #-}

module System.Directory.Internal.Common.AbstractFilePath where

import System.AbstractFilePath (unpackAFP, packAFP, unsafeFromChar)
import qualified System.AbstractFilePath as AFP

import qualified System.File.AbstractFilePath as P

import System.OsString.Internal.Types
import System.AbstractFilePath.Types
import System.AbstractFilePath.Data.ByteString.Short.Decode

#define SYSTEM_FILEPATH_MODULE System.AbstractFilePath
#define FILEPATH AbstractFilePath
#include "Template.hs"

unpack :: AbstractFilePath -> [OsChar]
unpack = unpackAFP

pack :: [OsChar] -> AbstractFilePath
pack = packAFP

-- UNSAFE... only use this with ascii, not unknown input
unsafeFromChar' :: Char -> OsChar
unsafeFromChar' = unsafeFromChar

toChar :: OsChar -> Char
toChar = AFP.toChar

unpackPlatform :: AbstractFilePath -> PlatformFilePath
unpackPlatform (OsString p) = p

packPlatform :: PlatformFilePath -> AbstractFilePath
packPlatform = OsString

-- for errors only, never fails
decodeFilepathFuzzy :: AbstractFilePath -> FilePath
#if defined(mingw32_HOST_OS)
decodeFilepathFuzzy (OsString (WS fp)) = decodeUtf8With lenientDecode fp
#else
decodeFilepathFuzzy (OsString (PS fp)) = decodeUtf8With lenientDecode fp
#endif


toString :: AbstractFilePath -> IO FilePath
toString = AFP.fromAbstractFilePathIO

fromStringIO :: String -> IO AbstractFilePath
fromStringIO = AFP.toAbstractFilePathIO

