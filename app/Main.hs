module Main where

import Brick
import Brick.BChan
import Cherry.Prelude
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Debug
import System.FilePath.Glob (Pattern)
import Lib
import Prelude (IO, (=<<), return, Maybe(..), FilePath, putStrLn, error)
import String
import System.FSNotify as FS
import System.Process (readProcessWithExitCode)
import qualified System.FilePath.Glob as Glob
import qualified Graphics.Vty as V

main :: IO ()
main = do
  chan <- newBChan 10
  void <| forkIO <| watchElmFiles chan

  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty

  void <| customMain initialVty buildVty (Prelude.Just chan) app initModel

watchElmFiles :: BChan Msg -> IO ()
watchElmFiles chan = do
  mgr <- FS.startManager
  -- TODO make the path to file-watch configurable
  void <| FS.watchTree mgr "client" shouldRecompile recompile
  where
    shouldRecompile :: Event -> Bool
    shouldRecompile event =
      isElmFile <| eventPath event

    isElmFile :: FilePath -> Bool
    isElmFile path =
      Glob.match elmFilePattern path

    recompile :: Event -> IO ()
    recompile event = do
      -- TODO make path to Elm configurable
      -- TODO make path to Main.elm configurable
      writeBChan chan <| Log <| "running elm make because of " ++ String.fromList (eventPath event)
      (exitCode, stdout, stderr) <- readProcessWithExitCode "elm" ["make", "client/app-monolithic/src/Entry.elm", "--output", "/dev/null"] ""
      writeBChan chan <| GotElmMakeOutput 
        ( exitCode
        , String.fromList stdout
        , String.fromList stderr
        )

elmFilePattern :: Pattern
elmFilePattern =
  Glob.compile "**/*.elm"
