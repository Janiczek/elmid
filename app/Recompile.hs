module Recompile (recompile) where

import Brick.BChan (BChan, writeBChan)
import Lib (Msg(..))
import NriPrelude
import System.Process (readProcessWithExitCode)
import Prelude (IO, FilePath)
import qualified Maybe

recompile :: Maybe FilePath -> BChan Msg -> IO ()
recompile path chan = do
  -- TODO make path to Elm configurable
  -- TODO make path to Main.elm configurable
  writeBChan chan <| RecompileStarted path
  result <- readProcessWithExitCode "elm" ["make", "client/app-monolithic/src/Entry.elm", "--output", "/dev/null"] ""
  writeBChan chan <| GotElmMakeOutput result
