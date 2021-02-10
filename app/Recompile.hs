module Recompile (recompile) where

import Brick.BChan (BChan, writeBChan)
import Lib (Msg(..))
import Cherry.Prelude
import System.Process (readProcessWithExitCode)
import Prelude (IO, FilePath)
import qualified String
import qualified Maybe

recompile :: Maybe FilePath -> BChan Msg -> IO ()
recompile path chan = do
  -- TODO make path to Elm configurable
  -- TODO make path to Main.elm configurable
  writeBChan chan <| RecompileStarted <| Maybe.map String.fromList path
  (exitCode, stdout, stderr) <- readProcessWithExitCode "elm" ["make", "client/app-monolithic/src/Entry.elm", "--output", "/dev/null"] ""
  writeBChan chan <| GotElmMakeOutput 
    ( exitCode
    , String.fromList stdout
    , String.fromList stderr
    )
