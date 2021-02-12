module Watch (watchElmFiles) where

import Brick.BChan (BChan)
import Control.Monad (forever, void, when)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Traversable (for)
import Flags (Flags (..))
import Lib (Msg)
import NriPrelude
import System.Directory (doesDirectoryExist)
import qualified System.Directory.Recursive as D
import System.FilePath.Glob (Pattern)
import qualified System.FilePath.Glob as Glob
import qualified System.Linux.Inotify as FS
import Prelude (FilePath, IO, return)


watchElmFiles :: Flags -> BChan Msg -> (Maybe FilePath -> BChan Msg -> IO ()) -> IO ()
watchElmFiles flags chan handleEvent = do
    inotify <- FS.init
    addWatchesRecursively inotify (fWatchedFolder flags)
    forever <| do
        event <- FS.getEvent inotify
        let path = T.unpack <| TE.decodeUtf8 <| FS.name event
        when (isElmFile path)
            <| handleEvent (Just path) chan


addWatchesRecursively :: FS.Inotify -> FilePath -> IO ()
addWatchesRecursively inotify dirpath = do
    _ <- FS.addWatch inotify dirpath FS.in_CLOSE_WRITE
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
