module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import Cherry.Prelude
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Lib (app, initModel)
import Prelude (Maybe(Just), (=<<), return)
import Recompile (recompile)
import Watch (watchElmFiles)
import qualified Graphics.Vty as V

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


