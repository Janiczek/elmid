module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Graphics.Vty as V
import Lib (app, initModel)
import NriPrelude
import Recompile (recompile)
import Watch (watchElmFiles)
import Prelude (IO, Maybe (Just), return, (=<<))


main :: IO ()
main = do
    chan <- newBChan 10
    recompile Nothing chan
    void <| forkIO <| watchElmFiles chan recompile

    let buildVty = do
            v <- V.mkVty =<< V.standardIOConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v

    initialVty <- buildVty

    void <| customMain initialVty buildVty (Prelude.Just chan) app initModel
