{-# LANGUAGE CPP #-}
module CurrentDirectory001 where
import qualified Data.List as List
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do
  prevDir <- getCurrentDirectory
  createDirectory "dir"
  setCurrentDirectory "dir"
  T(expectEq) () [".", ".."] . List.sort =<< getDirectoryContents "."
  setCurrentDirectory prevDir
  removeDirectory "dir"
