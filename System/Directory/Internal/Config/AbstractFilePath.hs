{-# LANGUAGE CPP #-}
module System.Directory.Internal.Config.AbstractFilePath where
#include <HsDirectoryConfig.h>

import System.OsString.Internal.Types
import Data.String ( fromString )

exeExtension :: OsString
exeExtension = fromString EXE_EXTENSION
-- We avoid using #const_str from hsc because it breaks cross-compilation
-- builds, so we use this ugly workaround where we simply paste the C string
-- literal directly in here.  This will probably break if the EXE_EXTENSION
-- contains strange characters, but hopefully no sane OS would ever do that.
