module System.Directory.Internal.Common.OsPath where

import qualified System.OsPath as OSP

import qualified System.File.OsPath as P

import System.OsString.Internal.Types
import System.OsPath.Types
import Prelude ()
import System.Directory.Internal.Prelude
import System.OsPath
  ( addTrailingPathSeparator
  , hasTrailingPathSeparator
  , isPathSeparator
  , isRelative
  , joinDrive
  , joinPath
  , normalise
  , pathSeparator
  , pathSeparators
  , splitDirectories
  , splitDrive
  , unsafeFromChar
  )
import GHC.IO.Encoding.UTF8
import GHC.IO.Encoding.UTF16
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.Exception (Exception(displayException))

-- | A generator with side-effects.
newtype ListT m a = ListT { unListT :: m (Maybe (a, ListT m a)) }

emptyListT :: Applicative m => ListT m a
emptyListT = ListT (pure Nothing)

maybeToListT :: Applicative m => m (Maybe a) -> ListT m a
maybeToListT m = ListT (((\ x -> (x, emptyListT)) <$>) <$> m)

listToListT :: Applicative m => [a] -> ListT m a
listToListT [] = emptyListT
listToListT (x : xs) = ListT (pure (Just (x, listToListT xs)))

liftJoinListT :: Monad m => m (ListT m a) -> ListT m a
liftJoinListT m = ListT (m >>= unListT)

listTHead :: Functor m => ListT m a -> m (Maybe a)
listTHead (ListT m) = (fst <$>) <$> m

listTToList :: Monad m => ListT m a -> m [a]
listTToList (ListT m) = do
  mx <- m
  case mx of
    Nothing -> return []
    Just (x, m') -> do
      xs <- listTToList m'
      return (x : xs)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM mx my = do
  x <- mx
  if x
    then my
    else return x

sequenceWithIOErrors_ :: [IO ()] -> IO ()
sequenceWithIOErrors_ actions = go (Right ()) actions
  where

    go :: Either IOError () -> [IO ()] -> IO ()
    go (Left e)   []       = ioError e
    go (Right ()) []       = pure ()
    go s          (m : ms) = s `seq` do
      r <- tryIOError m
      go (thenEither s r) ms

    -- equivalent to (*>) for Either, defined here to retain compatibility
    -- with base prior to 4.3
    thenEither :: Either b a -> Either b a -> Either b a
    thenEither x@(Left _) _ = x
    thenEither _          y = y

-- | Similar to 'try' but only catches a specify kind of 'IOError' as
--   specified by the predicate.
tryIOErrorType :: (IOError -> Bool) -> IO a -> IO (Either IOError a)
tryIOErrorType check action = do
  result <- tryIOError action
  case result of
    Left  err -> if check err then pure (Left err) else throwIO err
    Right val -> pure (Right val)

-- | Attempt to perform the given action, silencing any IO exception thrown by
-- it.
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catchIOError` (\_ -> pure ())

specializeErrorString :: String -> (IOError -> Bool) -> IO a -> IO a
specializeErrorString str errType action = do
  mx <- tryIOErrorType errType action
  case mx of
    Left  e -> throwIO (ioeSetErrorString e str)
    Right x -> pure x

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = ioeGetLocation e

-- | Given a list of path segments, expand @.@ and @..@.  The path segments
-- must not contain path separators.
expandDots :: [OsPath] -> [OsPath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs ->
          if | x == encodeFilepathUnsafe "." -> go ys' xs
             | x == encodeFilepathUnsafe ".." ->
                case ys' of
                  [] -> go (x : ys') xs
                  y : ys -> if y == encodeFilepathUnsafe ".." then go (x : ys') xs else go ys xs
             | otherwise -> go (x : ys') xs

-- | Convert to the right kind of slashes.
normalisePathSeps :: OsPath -> OsPath
normalisePathSeps p = pack $ (\ c -> if isPathSeparator c then pathSeparator else c) <$> unpack p

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: OsPath -> OsPath
normaliseTrailingSep (unpack -> path) = pack $ do
  let path' = reverse path
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  reverse (addSep path'')

-- | Convert empty paths to the current directory, otherwise leave it
-- unchanged.
emptyToCurDir :: OsPath -> OsPath
emptyToCurDir path
  | null (unpack path) = encodeFilepathUnsafe "."
  | otherwise = path

-- | Similar to 'normalise' but empty paths stay empty.
simplifyPosix :: OsPath -> OsPath
simplifyPosix path
  | null (unpack path) = pack []
  | otherwise = normalise path

-- | Similar to 'normalise' but:
--
-- * empty paths stay empty,
-- * parent dirs (@..@) are expanded, and
-- * paths starting with @\\\\?\\@ are preserved.
--
-- The goal is to preserve the meaning of paths better than 'normalise'.
simplifyWindows :: OsPath -> OsPath
simplifyWindows path
  | null (unpack path) = pack []
  | otherwise =
      if drive' == encodeFilepathUnsafe "\\\\?\\" then drive' <> subpath else simplifiedPath
  where
    simplifiedPath = joinDrive drive' subpath'
    (drive, subpath) = splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    upperDrive d = case unpack d of
      c : k : s | isAlpha (toChar c)
                , (toChar k) == ':'
                -- unsafeFromChar' is safe here, all chars are ascii
                , all isPathSeparator s -> pack (unsafeFromChar' (toUpper (toChar c)) : unsafeFromChar' ':' : s)
      _ -> d
    skipSeps = fmap pack . filter (not . (`elem` (pure <$> pathSeparators))) . fmap unpack
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== encodeFilepathUnsafe "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (pack [pathSeparator] <>)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
                 && (null (unpack drive) || hasTrailingPathSep) -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep
                  && not (pathIsAbsolute && null (unpack p))
                  = addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (isRelative path)
    subpathIsAbsolute = any isPathSeparator (take 1 $ unpack subpath)
    hasTrailingPathSep = hasTrailingPathSeparator subpath

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only: directory link
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. 'System.Directory.removeDirectory' vs 'System.Directory.removeFile'.
fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

-- | Return whether the given 'FileType' is a link.
fileTypeIsLink :: FileType -> Bool
fileTypeIsLink SymbolicLink  = True
fileTypeIsLink DirectoryLink = True
fileTypeIsLink _             = False

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)

-- | Truncate the destination file and then copy the contents of the source
-- file to the destination file.  If the destination file already exists, its
-- attributes shall remain unchanged.  Otherwise, its attributes are reset to
-- the defaults.
copyFileContents :: OsPath            -- ^ Source filename
                 -> OsPath            -- ^ Destination filename
                 -> IO ()
copyFileContents fromFPath toFPath =
  (`ioeAddLocation` "copyFileContents") `modifyIOError` do
    P.withBinaryFile toFPath WriteMode $ \ hTo ->
      copyFileToHandle fromFPath hTo

-- | Copy all data from a file to a handle.
copyFileToHandle :: OsPath            -- ^ Source file
                 -> Handle              -- ^ Destination handle
                 -> IO ()
copyFileToHandle fromFPath hTo =
  (`ioeAddLocation` "copyFileToHandle") `modifyIOError` do
    P.withBinaryFile fromFPath ReadMode $ \ hFrom ->
      copyHandleData hFrom hTo

-- | Copy data from one handle to another until end of file.
copyHandleData :: Handle                -- ^ Source handle
               -> Handle                -- ^ Destination handle
               -> IO ()
copyHandleData hFrom hTo =
  (`ioeAddLocation` "copyData") `modifyIOError` do
    allocaBytes bufferSize go
  where
    bufferSize = 131072 -- 128 KiB, as coreutils `cp` uses as of May 2014 (see ioblksize.h)
    go buffer = do
      count <- hGetBuf hFrom buffer bufferSize
      when (count > 0) $ do
        hPutBuf hTo buffer count
        go buffer

-- | Special directories for storing user-specific application data,
-- configuration, and cache files, as specified by the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
-- Note: On Windows, 'XdgData' and 'XdgConfig' usually map to the same
-- directory.
--
-- @since 1.2.3.0
data XdgDirectory
  = XdgData
    -- ^ For data files (e.g. images).
    -- It uses the @XDG_DATA_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.local\/share@.
    -- On Windows, the default is @%APPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    -- Can be considered as the user-specific equivalent of @\/usr\/share@.
  | XdgConfig
    -- ^ For configuration files.
    -- It uses the @XDG_CONFIG_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.config@.
    -- On Windows, the default is @%APPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    -- Can be considered as the user-specific equivalent of @\/etc@.
  | XdgCache
    -- ^ For non-essential files (e.g. cache).
    -- It uses the @XDG_CACHE_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.cache@.
    -- On Windows, the default is @%LOCALAPPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
    -- Can be considered as the user-specific equivalent of @\/var\/cache@.
  | XdgState
   -- ^ For data that should persist between (application) restarts,
   -- but that is not important or portable enough to the user that it
   -- should be stored in 'XdgData'.
   -- It uses the @XDG_STATE_HOME@ environment variable.
   -- On non-Windows sytems, the default is @~\/.local\/state@.  On
   -- Windows, the default is @%LOCALAPPDATA%@
   -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
   --
   -- @since 1.3.7.0
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Search paths for various application data, as specified by the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
-- The list of paths is split using 'System.FilePath.searchPathSeparator',
-- which on Windows is a semicolon.
--
-- Note: On Windows, 'XdgDataDirs' and 'XdgConfigDirs' usually yield the same
-- result.
--
-- @since 1.3.2.0
data XdgDirectoryList
  = XdgDataDirs
    -- ^ For data files (e.g. images).
    -- It uses the @XDG_DATA_DIRS@ environment variable.
    -- On non-Windows systems, the default is @\/usr\/local\/share\/@ and
    -- @\/usr\/share\/@.
    -- On Windows, the default is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    -- (e.g. @C:\/ProgramData@).
  | XdgConfigDirs
    -- ^ For configuration files.
    -- It uses the @XDG_CONFIG_DIRS@ environment variable.
    -- On non-Windows systems, the default is @\/etc\/xdg@.
    -- On Windows, the default is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    -- (e.g. @C:\/ProgramData@).
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

unpack :: OsPath -> [OsChar]
unpack = OSP.unpack

pack :: [OsChar] -> OsPath
pack = OSP.pack

-- UNSAFE... only use this with ascii, not unknown input
unsafeFromChar' :: Char -> OsChar
unsafeFromChar' = unsafeFromChar

toChar :: OsChar -> Char
toChar = OSP.toChar

unpackPlatform :: OsPath -> PlatformPath
unpackPlatform (OsString p) = p

packPlatform :: PlatformPath -> OsPath
packPlatform = OsString

-- for errors only, never fails
decodeFilepathFuzzy :: OsPath -> FilePath
decodeFilepathFuzzy = either (error . displayException) id . OSP.decodeWith (mkUTF8 TransliterateCodingFailure) (mkUTF16le TransliterateCodingFailure)

encodeFilepathUnsafe :: FilePath -> OsPath
encodeFilepathUnsafe =
  either (error . displayException) id
  . OSP.encodeWith
    (mkUTF8 ErrorOnCodingFailure)
    (mkUTF16le ErrorOnCodingFailure)

decodeFilepathUnsafe :: OsPath -> FilePath
decodeFilepathUnsafe =
  either (error . displayException) id
  . OSP.decodeWith
    (mkUTF8 ErrorOnCodingFailure)
    (mkUTF16le ErrorOnCodingFailure)


toString :: OsPath -> IO FilePath
toString = OSP.decodeFS

fromStringIO :: String -> IO OsPath
fromStringIO = OSP.encodeFS

