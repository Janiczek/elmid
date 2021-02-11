module Recompile (recompile) where

import Brick.BChan (BChan, writeBChan)
import Flags
import Lib (Msg (..))
import qualified Maybe
import NriPrelude
import System.Process (readProcessWithExitCode)
import Prelude (FilePath, IO)


recompile :: Flags -> Maybe FilePath -> BChan Msg -> IO ()
recompile flags path chan = do
    writeBChan chan <| RecompileStarted path
    result <-
        readProcessWithExitCode
            (fElmPath flags)
            [ "make"
            , fMainPath flags
            , "--output"
            , "/dev/null"
            ]
            ""
    writeBChan chan <| GotElmMakeOutput result
