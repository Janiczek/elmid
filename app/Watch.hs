module Watch (watchElmFiles) where

import Data.Traversable (for)
import Brick.BChan (BChan)
import Lib (Msg)
import System.Directory (doesDirectoryExist)
import Control.Monad (void, forever, when)
import NriPrelude
import System.FilePath.Glob (Pattern)
import Prelude (IO, FilePath, return)
import qualified System.FilePath.Glob as Glob
import qualified System.Directory.Recursive as D
import qualified System.Linux.Inotify as FS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

watchElmFiles :: BChan Msg -> (Maybe FilePath -> BChan Msg -> IO ()) -> IO ()
watchElmFiles chan handleEvent = do
  inotify <- FS.init
  -- TODO make the path to file-watch configurable
  addWatchesRecursively inotify "client"
  forever <| do
    event <- FS.getEvent inotify
    let path = T.unpack <| TE.decodeUtf8 <| FS.name event
    when (isElmFile path) <|
      handleEvent (Just path) chan

addWatchesRecursively :: FS.Inotify -> FilePath -> IO ()
addWatchesRecursively inotify dirpath = do
  FS.addWatch inotify dirpath FS.in_CLOSE_WRITE
  subdirs <- D.getDirFiltered shouldRecurse dirpath
  void <| for subdirs <| \subdir -> FS.addWatch inotify subdir FS.in_CLOSE_WRITE

shouldRecurse :: FilePath -> IO Bool
shouldRecurse path = do
  isDir <- doesDirectoryExist path
  let isInteresting = not <| Glob.match ignoredFolderPattern path
  return <| isDir && isInteresting

isElmFile :: FilePath -> Bool
isElmFile path =
  Glob.match elmFilePattern path

ignoredFolderPattern :: Pattern
ignoredFolderPattern =
  -- TODO use the `ignore` package instead
  -- https://hackage.haskell.org/package/ignore-0.1.1.0/docs/Ignore.html
  Glob.compile "**/node_modules"

elmFilePattern :: Pattern
elmFilePattern =
  Glob.compile "**/*.elm"
