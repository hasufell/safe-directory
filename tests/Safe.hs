-- Verify that System.Directory is indeed Safe (regression test for issue #30)
module Safe where
import System.Directory ()

main :: a -> IO ()
main _ = return ()
