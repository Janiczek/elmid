module Watch (watchElmFiles) where

import Brick.BChan (BChan)
import Control.Monad (forever, void, when)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Traversable (for)
import Flags (Flags (..))
import Lib (Msg)
import NriPrelude
import System.FilePath.Glob (Pattern)
import qualified System.FilePath.Glob as Glob
import qualified System.Linux.Inotify as FS
import qualified System.Posix.Files as Posix
import qualified System.Posix.Recursive as Recursive
import Prelude (FilePath, IO, length, drop, otherwise, any)


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
    subdirs <- Recursive.listAccessible fsRecurseConf dirpath
    void <| for subdirs <| \subdir -> FS.addWatch inotify subdir FS.in_CLOSE_WRITE
  where
    fsRecurseConf :: Recursive.Conf
    fsRecurseConf =
        Recursive.Conf
            { Recursive.preCheck = \path -> not <| any (path `isSuffixOf`) ignoredDirs
            , Recursive.postCheck = \f _ -> Posix.isDirectory f
            , Recursive.followSymlinks = False
            }

    ignoredDirs :: [FilePath]
    ignoredDirs = ["node_modules", "elm-stuff"]


isSuffixOf :: FilePath -> FilePath -> Bool
isSuffixOf needle haystack
    | needleLen > hayLen = False
    | otherwise = needle == drop (hayLen - needleLen) haystack
  where
    needleLen = length needle
    hayLen =length haystack


isElmFile :: FilePath -> Bool
isElmFile path =
    Glob.match elmFilePattern path


elmFilePattern :: Pattern
elmFilePattern =
    Glob.compile "**/*.elm"
