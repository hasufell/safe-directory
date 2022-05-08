{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4, 8, 0)
-- In base-4.8.0 the Foreign module became Safe
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory.AbstractFilePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory.AbstractFilePath
   (
    -- $intro

    -- * Types
      AbstractFilePath
    , OsString

    -- * Actions on directories
    , createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , renameDirectory
    , listDirectory
    , getDirectoryContents
    -- ** Current working directory
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , XdgDirectory(..)
    , getXdgDirectory
    , XdgDirectoryList(..)
    , getXdgDirectoryList
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata
    , getFileSize

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory

    -- * Existence tests
    , doesPathExist
    , doesFileExist
    , doesDirectoryExist

    , findExecutable
    , findExecutables
    , findExecutablesInDirectories
    , findFile
    , findFiles
    , findFileWith
    , findFilesWith
    , exeExtension

    -- * Symbolic links
    , createFileLink
    , createDirectoryLink
    , removeDirectoryLink
    , pathIsSymbolicLink
    , getSymbolicLinkTarget

    -- * Permissions

    -- $permissions

    , Permissions
    , emptyPermissions
    , readable
    , writable
    , executable
    , searchable
    , setOwnerReadable
    , setOwnerWritable
    , setOwnerExecutable
    , setOwnerSearchable

    , getPermissions
    , setPermissions
    , copyPermissions

    -- * Timestamps

    , getAccessTime
    , getModificationTime
    , setAccessTime
    , setModificationTime

    -- * Deprecated
    , isSymbolicLink

   ) where
import Prelude ()
import System.Directory.Internal.AbstractFilePath
import System.Directory.Internal.Prelude hiding (lookupEnv)
import System.AbstractFilePath
  ( (<.>)
  , (</>)
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
  , hasTrailingPathSeparator
  , isAbsolute
  , joinPath
  , makeRelative
  , splitDirectories
  , splitSearchPath
  , takeDirectory
  , AbstractFilePath
  , OsString
  )
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.String ( fromString )


#define FILEPATH AbstractFilePath
#define STRING OsString
#include "Template.hs"
