#define T(f) (T.f _t __FILE__ __LINE__)

import Prelude ()
import System.Directory.Internal.Prelude hiding ( writeFile, readFile )
import System.Directory.OsPath
import Util (TestEnv)
import qualified Util as T
-- This comment prevents "T" above from being treated as the function-like
-- macro defined earlier.
import System.Directory.Internal.Common.OsPath ( encodeFilepathUnsafe, decodeFilepathUnsafe )
import Data.String
import System.File.OsPath ( writeFile, readFile, writeFile', readFile' )
import System.OsString.Internal.Types
import TestUtils (toBS)
