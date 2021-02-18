module Recompile (recompile) where

import Brick.BChan (BChan, writeBChan)
import Data.Maybe (fromJust)
import Flags (Flags (..))
import GHC.IO.Handle as H
import Lib (Msg (..))
import NriPrelude
import System.Process as P
import Prelude (FilePath, IO)


recompile :: Flags -> Maybe FilePath -> BChan Msg -> IO ()
recompile flags path chan = do
    let mainCwd = case fMainCwd flags of
            "." -> Nothing
            cwd_ -> Just cwd_
    writeBChan chan <| RecompileStarted path
    (_, maybeStdoutH, _, processH) <-
        P.createProcess_
            "recompile"
            ( (shell ("script -qefc \"" ++ fElmPath flags ++ " make " ++ fMainPath flags ++ " --output /dev/null\" /dev/null"))
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = mainCwd
                }
            )
    let stdoutH = fromJust maybeStdoutH
    exitCode <- P.waitForProcess processH
    stdout <- H.hGetContents stdoutH
    writeBChan chan <| GotElmMakeOutput exitCode stdout
