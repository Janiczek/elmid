module Watch (watchElmFiles) where

import Brick.BChan (BChan)
import Control.Monad (forever, void, when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Traversable (for)
import Flags (Flags (..))
import Lib (Msg)
import NriPrelude
import qualified System.Linux.Inotify as FS
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Files.ByteString as Posix
import qualified System.Posix.Recursive.ByteString as Recursive
import Prelude (FilePath, IO, any)


watchElmFiles :: Flags -> BChan Msg -> (Maybe FilePath -> BChan Msg -> IO ()) -> IO ()
watchElmFiles flags chan handleEvent = do
    inotify <- FS.init
    addWatchesRecursively inotify (fWatchedFolder flags)
    forever <| do
        event <- FS.getEvent inotify
        let path = FS.name event
        when (".elm" `BS.isSuffixOf` path)
            <| handleEvent (Just <| T.unpack <| TE.decodeUtf8 <| path) chan


addWatchesRecursively :: FS.Inotify -> RawFilePath -> IO ()
addWatchesRecursively inotify dirpath = do
    allDirs <- Recursive.listAccessible fsRecurseConf dirpath
    void <| for allDirs <| \dir -> FS.addWatch_ inotify dir FS.in_CLOSE_WRITE
  where
    fsRecurseConf :: Recursive.Conf
    fsRecurseConf =
        Recursive.Conf
            { Recursive.preCheck = \path -> not <| any (path `BS.isSuffixOf`) ignoredDirs
            , Recursive.postCheck = \f _ -> Posix.isDirectory f
            , Recursive.followSymlinks = False
            }

    ignoredDirs :: [RawFilePath]
    ignoredDirs = ["node_modules", "elm-stuff"]
