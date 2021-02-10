module Main where

import Brick
import Brick.BChan
import Cherry.Prelude
import Control.Concurrent (forkIO, threadDelay)
import Data.Text as T
import Data.Text.Encoding as TE
import Control.Monad (void, forever, when)
import Debug
import System.FilePath.Glob (Pattern)
import Lib
import Prelude (IO, String, (=<<), return, Maybe(..), FilePath, putStrLn, error)
import String
import System.Linux.Inotify as FS
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

isElmFile :: FilePath -> Bool
isElmFile path =
  Glob.match elmFilePattern path

elmFilePattern :: Pattern
elmFilePattern =
  Glob.compile "**/*.elm"

watchElmFiles :: BChan Msg -> IO ()
watchElmFiles chan = do
  inotify <- FS.init
  -- TODO make the path to file-watch configurable
  FS.addWatch inotify "client" FS.in_CLOSE_WRITE
  forever <| do
    event <- FS.getEvent inotify
    let path = T.unpack <| TE.decodeUtf8 <| FS.name event
    when (isElmFile path) <|
      recompile path chan

recompile :: Prelude.String -> BChan Msg -> IO ()
recompile path chan = do
  -- TODO make path to Elm configurable
  -- TODO make path to Main.elm configurable
  writeBChan chan <| Log <| "running elm make because of " ++ String.fromList path
  (exitCode, stdout, stderr) <- readProcessWithExitCode "elm" ["make", "client/app-monolithic/src/Entry.elm", "--output", "/dev/null"] ""
  writeBChan chan <| GotElmMakeOutput 
    ( exitCode
    , String.fromList stdout
    , String.fromList stderr
    )
