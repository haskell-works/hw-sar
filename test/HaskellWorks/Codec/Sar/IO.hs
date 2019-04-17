module HaskellWorks.Codec.Sar.IO
  ( run
  ) where

import qualified System.Exit    as IO
import qualified System.Process as IO

run :: String -> IO IO.ExitCode
run cmd = do
  p <- IO.runCommand cmd
  IO.waitForProcess p
