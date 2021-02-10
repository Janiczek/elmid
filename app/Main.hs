module Main where

import Brick
import Brick.BChan
import Cherry.Prelude
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Lib
import Prelude (IO, (=<<), return, Maybe(Nothing), FilePath)
import Debug
import String
import System.Process (readProcess)
import Twitch ((|+),(|%),(|-))
import qualified Graphics.Vty as V
import qualified Twitch as T

main :: IO ()
main = do
  chan <- newBChan 10
  void <| forkIO <| forever <| watchElmFiles chan

  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty

  void <| customMain initialVty buildVty Prelude.Nothing app initModel

watchElmFiles :: BChan String -> IO ()
watchElmFiles chan =
  T.defaultMain <| do
    -- TODO this *.elm path configurable
    "client/*.elm" |+ runElmMake -- on add
                   |% runElmMake -- on modify
                   |- runElmMake -- on delete
      where
        runElmMake :: FilePath -> IO ()
        runElmMake _ = do
          -- TODO make path to Elm configurable
          -- TODO make path to Main.elm configurable
          stdout <- readProcess "elm" ["make", "client/app-monolithic/src/Entry.elm", "--output", "/dev/null"] ""
          writeBChan chan (String.fromList stdout)


