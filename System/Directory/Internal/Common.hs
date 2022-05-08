{-# LANGUAGE CPP #-}

module System.Directory.Internal.Common where

import qualified System.Directory.Internal.Prelude as P

#define SYSTEM_FILEPATH_MODULE System.FilePath
#define FILEPATH FilePath
#include "Common/Template.hs"

unpack :: FilePath -> [Char]
unpack = id

pack :: [Char] -> FilePath
pack = id

-- UNSAFE... only use this with ascii, not unknown input
unsafeFromChar' :: Char -> Char
unsafeFromChar' = id

toChar :: Char -> Char
toChar = id

unpackPlatform :: FilePath -> FilePath
unpackPlatform = id

packPlatform :: FilePath -> FilePath
packPlatform = id

-- for errors only, never fails
decodeFilepathFuzzy :: FilePath -> FilePath
decodeFilepathFuzzy = id

toString :: FilePath -> IO FilePath
toString = pure

fromStringIO :: String -> IO String
fromStringIO = pure
