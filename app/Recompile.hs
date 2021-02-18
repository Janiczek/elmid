module Recompile (recompile) where

import Brick.BChan (BChan, writeBChan)
import Flags (Flags (..))
import GHC.IO.Handle as H
import Lib (Msg (..))
import NriPrelude
import System.Process as P
import Prelude (FilePath, IO)


recompile :: Flags -> Maybe FilePath -> BChan Msg -> IO ()
recompile flags path chan = do
    writeBChan chan <| RecompileStarted path
    (_, stdoutH, _, processH) <- P.runInteractiveCommand <| "script -qefc \"" ++ fElmPath flags ++ " make " ++ fMainPath flags ++ " --output /dev/null\" /dev/null"
    exitCode <- P.waitForProcess processH
    stdout <- H.hGetContents stdoutH
    writeBChan chan <| GotElmMakeOutput exitCode stdout
