module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Flags
import qualified Graphics.Vty as V
import Lib (app, initModel)
import NriPrelude
import qualified Options.Applicative as O
import Recompile (recompile)
import Watch (watchElmFiles)
import Prelude (IO, Maybe (Just), return, (=<<))


main :: IO ()
main = do
    flags <- O.execParser Flags.opts
    chan <- newBChan 10
    recompile flags Nothing chan
    void <| forkIO <| watchElmFiles flags chan (recompile flags)

    let buildVty = do
            v <- V.mkVty =<< V.standardIOConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v

    initialVty <- buildVty

    void <| customMain initialVty buildVty (Prelude.Just chan) app initModel
