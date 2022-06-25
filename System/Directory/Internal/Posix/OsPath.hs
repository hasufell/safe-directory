{-# LANGUAGE CPP #-}

module System.Directory.Internal.Posix.OsPath where

#if !defined(mingw32_HOST_OS)
import System.Directory.Internal.PosixFFI
import qualified System.OsPath.Data.ByteString.Short as L
import System.Directory.Internal.Common.OsPath

import System.OsPath hiding ( decodeFS )
import System.OsString.Internal.Types
import System.OsPath.Posix (decodeFS)
import System.Directory.Internal.Config.OsPath (exeExtension)

import qualified System.Posix.Env.PosixString as PS
import qualified System.Posix.Directory.PosixPath as Posix
import qualified System.Posix.Files.PosixString as Posix

import System.IO.Error (doesNotExistErrorType)

import Prelude ()
import System.Directory.Internal.Prelude hiding (lookupEnv, getEnv)
#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified System.Posix.Types as Posix
import qualified System.Posix.User as PU

createDirectoryInternal :: OsPath -> IO ()
createDirectoryInternal path = Posix.createDirectory (unpackPlatform path) 0o777

removePathInternal :: Bool -> OsPath -> IO ()
removePathInternal True  = Posix.removeDirectory . unpackPlatform
removePathInternal False = Posix.removeLink . unpackPlatform

renamePathInternal :: OsPath -> OsPath -> IO ()
renamePathInternal f t = Posix.rename (unpackPlatform f) (unpackPlatform t)

-- | On POSIX, equivalent to 'simplifyPosix'.
simplify :: OsPath -> OsPath
simplify = simplifyPosix


c_PATH_MAX :: Maybe Int
#ifdef PATH_MAX
c_PATH_MAX | c_PATH_MAX' > toInteger maxValue = Nothing
           | otherwise                        = Just (fromInteger c_PATH_MAX')
  where c_PATH_MAX' = (#const PATH_MAX)
        maxValue = maxBound `asTypeInMaybe` c_PATH_MAX
        asTypeInMaybe :: a -> Maybe a -> a
        asTypeInMaybe = const
#else
c_PATH_MAX = Nothing
#endif

withRealpath :: CString -> (CString -> IO a) -> IO a
withRealpath path action = case c_PATH_MAX of
  Nothing ->
    -- newer versions of POSIX support cases where the 2nd arg is NULL;
    -- hopefully that is the case here, as there is no safer way
    bracket (realpath nullPtr) c_free action
  Just pathMax ->
    -- allocate one extra just to be safe
    allocaBytes (pathMax + 1) (realpath >=> action)
  where realpath = throwErrnoIfNull "" . c_realpath path

canonicalizePathSimplify :: OsPath -> IO OsPath
canonicalizePathSimplify = pure

findExecutablesLazyInternal :: ([OsPath] -> OsString -> ListT IO OsPath)
                            -> OsString
                            -> ListT IO OsPath
findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary =
  liftJoinListT $ do
    path <- getPath
    pure (findExecutablesInDirectoriesLazy path binary)

exeExtensionInternal :: OsString
exeExtensionInternal = exeExtension

getDirectoryContentsInternal :: OsPath -> IO [OsPath]
getDirectoryContentsInternal path =
  fmap packPlatform <$> bracket
    (Posix.openDirStream $ unpackPlatform path)
    Posix.closeDirStream
    start
  where
    start dirp = loop id
      where
        loop acc = do
          e <- Posix.readDirStream dirp
          if OsString e == encodeFilepathUnsafe ""
            then pure (acc [])
            else loop (acc . (e:))

getCurrentDirectoryInternal :: IO OsPath
getCurrentDirectoryInternal = packPlatform <$> Posix.getWorkingDirectory

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended and the path may or may not be simplified.
-- If the path is already absolute, the path is returned unchanged.  The
-- function preserves the presence or absence of the trailing path separator.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may throw exceptions.
--
-- Empty paths are treated as the current directory.
prependCurrentDirectory :: OsPath -> IO OsPath
prependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetFileName` decodeFilepathFuzzy path)) `modifyIOError` do
      (</> path) <$> getCurrentDirectoryInternal
  | otherwise = pure path

setCurrentDirectoryInternal :: OsPath -> IO ()
setCurrentDirectoryInternal = Posix.changeWorkingDirectory . unpackPlatform

linkToDirectoryIsDirectory :: Bool
linkToDirectoryIsDirectory = False

createSymbolicLink :: Bool -> OsPath -> OsPath -> IO ()
createSymbolicLink _ f t = Posix.createSymbolicLink (unpackPlatform f) (unpackPlatform t)

readSymbolicLink :: OsPath -> IO OsPath
readSymbolicLink = fmap packPlatform . Posix.readSymbolicLink . unpackPlatform

type Metadata = Posix.FileStatus

getSymbolicLinkMetadata :: OsPath -> IO Metadata
getSymbolicLinkMetadata = Posix.getSymbolicLinkStatus . unpackPlatform

getFileMetadata :: OsPath -> IO Metadata
getFileMetadata = Posix.getFileStatus . unpackPlatform

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata stat
  | isLink    = SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = Posix.isSymbolicLink stat
    isDir  = Posix.isDirectory stat

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = fromIntegral . Posix.fileSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . posix_accessTimeHiRes

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . posix_modificationTimeHiRes

posix_accessTimeHiRes, posix_modificationTimeHiRes
  :: Posix.FileStatus -> POSIXTime
#if MIN_VERSION_unix(2, 6, 0)
posix_accessTimeHiRes = Posix.accessTimeHiRes
posix_modificationTimeHiRes = Posix.modificationTimeHiRes
#else
posix_accessTimeHiRes = realToFrac . Posix.accessTime
posix_modificationTimeHiRes = realToFrac . Posix.modificationTime
#endif

type Mode = Posix.FileMode

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Posix.fileMode

allWriteMode :: Posix.FileMode
allWriteMode =
  Posix.ownerWriteMode .|.
  Posix.groupWriteMode .|.
  Posix.otherWriteMode

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. allWriteMode /= 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .&. complement allWriteMode
setWriteMode True  m = m .|. allWriteMode

setFileMode :: OsPath -> Mode -> IO ()
setFileMode = Posix.setFileMode . unpackPlatform

setFilePermissions :: OsPath -> Mode -> IO ()
setFilePermissions = setFileMode

getAccessPermissions :: OsPath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
  r <- Posix.fileAccess (unpackPlatform path) True  False False
  w <- Posix.fileAccess (unpackPlatform path) False True  False
  x <- Posix.fileAccess (unpackPlatform path) False False True
  pure Permissions
       { readable   = r
       , writable   = w
       , executable = x && not isDir
       , searchable = x && isDir
       }

setAccessPermissions :: OsPath -> Permissions -> IO ()
setAccessPermissions path (Permissions r w e s) = do
  m <- getFileMetadata path
  setFileMode path (modifyBit (e || s) Posix.ownerExecuteMode .
                    modifyBit w Posix.ownerWriteMode .
                    modifyBit r Posix.ownerReadMode .
                    modeFromMetadata $ m)
  where
    modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b

copyOwnerFromStatus :: Posix.FileStatus -> OsPath -> IO ()
copyOwnerFromStatus st dst = do
  Posix.setOwnerAndGroup (unpackPlatform dst) (Posix.fileOwner st) (-1)

copyGroupFromStatus :: Posix.FileStatus -> OsPath -> IO ()
copyGroupFromStatus st dst = do
  Posix.setOwnerAndGroup (unpackPlatform dst) (-1) (Posix.fileGroup st)

tryCopyOwnerAndGroupFromStatus :: Posix.FileStatus -> OsPath -> IO ()
tryCopyOwnerAndGroupFromStatus st dst = do
  ignoreIOExceptions (copyOwnerFromStatus st dst)
  ignoreIOExceptions (copyGroupFromStatus st dst)

copyFileWithMetadataInternal :: (Metadata -> OsPath -> IO ())
                             -> (Metadata -> OsPath -> IO ())
                             -> OsPath
                             -> OsPath
                             -> IO ()
copyFileWithMetadataInternal copyPermissionsFromMetadata
                             copyTimesFromMetadata
                             src
                             dst = do
  st <- Posix.getFileStatus (unpackPlatform src)
  copyFileContents src dst
  tryCopyOwnerAndGroupFromStatus st dst
  copyPermissionsFromMetadata st dst
  copyTimesFromMetadata st dst

setTimes :: OsPath -> (Maybe POSIXTime, Maybe POSIXTime) -> IO ()
#ifdef HAVE_UTIMENSAT
setTimes path' (atime', mtime') =
  withFilePath path' $ \ path'' ->
  withArray [ maybe utimeOmit toCTimeSpec atime'
            , maybe utimeOmit toCTimeSpec mtime' ] $ \ times ->
  throwErrnoPathIfMinus1_ "" path' $
    c_utimensat c_AT_FDCWD path'' times 0
#else
setTimes path' (Just atime', Just mtime') = setFileTimes' path' atime' mtime'
setTimes path' (atime', mtime') = do
  m <- getFileMetadata path'
  let atimeOld = accessTimeFromMetadata m
  let mtimeOld = modificationTimeFromMetadata m
  setFileTimes' path'
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds atimeOld) atime')
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds mtimeOld) mtime')

setFileTimes' ::
  OsPath -> POSIXTime -> POSIXTime -> IO ()
# if MIN_VERSION_unix(2, 7, 0)
setFileTimes' pth atime' mtime' = Posix.setFileTimesHiRes (unpackPlatform pth) atime' mtime'
#  else
setFileTimes' pth atime' mtime' =
  Posix.setFileTimes pth
    (fromInteger (truncate atime'))
    (fromInteger (truncate mtime'))
# endif
#endif

-- | Get the contents of the @PATH@ environment variable.
getPath :: IO [OsPath]
getPath = splitSearchPath <$> getEnv (encodeFilepathUnsafe "PATH")

-- | $HOME is preferred, because the user has control over it. However, POSIX
-- doesn't define it as a mandatory variable, so fall back to `getpwuid_r`.
getHomeDirectoryInternal :: IO OsPath
getHomeDirectoryInternal = do
  e <- lookupEnv (encodeFilepathUnsafe "HOME")
  case e of
       Just fp -> pure fp
       -- TODO: encodeFilepathUnsafe here is bad, but unix's System.Posix.User.UserEntry does not have ByteString/OsString variants
       Nothing -> (PU.homeDirectory <$> (PU.getEffectiveUserID >>= PU.getUserEntryForID)) >>= encodeFS

getXdgDirectoryFallback :: IO OsPath -> XdgDirectory -> IO OsPath
getXdgDirectoryFallback getHomeDirectory xdgDir = do
  (<$> getHomeDirectory) $ flip (</>) $ case xdgDir of
    XdgData   -> encodeFilepathUnsafe ".local/share"
    XdgConfig -> encodeFilepathUnsafe ".config"
    XdgCache  -> encodeFilepathUnsafe ".cache"
    XdgState  -> encodeFilepathUnsafe ".local/state"

getXdgDirectoryListFallback :: XdgDirectoryList -> IO [OsPath]
getXdgDirectoryListFallback xdgDirs =
  pure $ case xdgDirs of
    XdgDataDirs   -> [encodeFilepathUnsafe "/usr/local/share/", encodeFilepathUnsafe "/usr/share/"]
    XdgConfigDirs -> [encodeFilepathUnsafe "/etc/xdg"]

getAppUserDataDirectoryInternal :: OsPath -> IO OsPath
getAppUserDataDirectoryInternal appName =
  (\ home -> home <> (encodeFilepathUnsafe "/" <> encodeFilepathUnsafe "." <> appName)) <$> getHomeDirectoryInternal

getUserDocumentsDirectoryInternal :: IO OsPath
getUserDocumentsDirectoryInternal = getHomeDirectoryInternal

getTemporaryDirectoryInternal :: IO OsPath
getTemporaryDirectoryInternal = fromMaybe (encodeFilepathUnsafe "/tmp") <$> lookupEnv (encodeFilepathUnsafe "TMPDIR")

lookupEnv :: OsString -> IO (Maybe OsString)
lookupEnv (OsString name@(PS _)) = fmap OsString <$> PS.getEnv name

getEnv :: OsString -> IO OsString
getEnv (OsString name@(PS _)) = do
  env <- PS.getEnv name
  pn <- decodeFS name
  case env of
    Nothing -> throwIO (mkIOError doesNotExistErrorType ("Env var '" <> pn <> "' could not be found!") Nothing Nothing)
    Just value -> pure (OsString value)


canonicalizePathWith :: ((OsPath -> IO OsPath) -> OsPath -> IO OsPath)
                     -> OsPath
                     -> IO OsPath
canonicalizePathWith attemptRealpath path = do
  let realpath (OsString (PS sb)) = L.useAsCString sb (\cstr -> withRealpath cstr (fmap (OsString . PS) . L.packCString))
  attemptRealpath realpath path
#endif
