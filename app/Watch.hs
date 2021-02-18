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
    addWatchesRecursively inotify flags
    forever <| do
        event <- FS.getEvent inotify
        let path = T.unpack <| TE.decodeUtf8 <| FS.name event
        when (isElmFile path)
            <| handleEvent (Just path) chan


elmFilePattern :: Pattern
elmFilePattern =
    Glob.compile "**/*.elm"


isElmFile :: FilePath -> Bool
isElmFile path =
    Glob.match elmFilePattern path
{-# INLINE isElmFile #-}


addWatchesRecursively :: FS.Inotify -> Flags -> IO ()
addWatchesRecursively inotify flags = do
    let watchedFolder = fWatchedFolder flags
    _ <- FS.addWatch inotify watchedFolder FS.in_CLOSE_WRITE
    subdirs <- D.getDirFiltered shouldRecurse watchedFolder
    void <| for subdirs <| \subdir -> FS.addWatch inotify subdir FS.in_CLOSE_WRITE
  where
    ignoredFolderPattern :: Pattern
    ignoredFolderPattern =
        Glob.compile <| "**/" ++ fIgnoredFolder flags
    {-# INLINE shouldRecurse #-}
    shouldRecurse :: FilePath -> IO Bool
    shouldRecurse path = do
        isDir <- doesDirectoryExist path
        let isInteresting = not <| Glob.match ignoredFolderPattern path
        return <| isDir && isInteresting
