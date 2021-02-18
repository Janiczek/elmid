module Flags where

import Data.Semigroup ((<>))
import NriPrelude
import Options.Applicative
import Prelude (String)


data Flags = Flags
    { fElmPath :: String
    , fWatchedFolder :: String
    , fIgnoredFolder :: String
    , fMainPath :: String
    }


flags :: Parser Flags
flags =
    Flags
        <$> strOption
            ( long "elm-path"
                <> metavar "ELM_PATH"
                <> help "Path to the `elm` compiler binary. Defaults to `elm`."
                <> value "elm"
            )
        <*> strOption
            ( long "watched-folder"
                <> metavar "FOLDER"
                <> help "Path to the watched folder (containing Elm files). Defaults to `.`."
                <> value "."
            )
        <*> strOption
            ( long "ignored-folder"
                <> metavar "FOLDER"
                <> help "Folder name to be ignored by the watcher (like `node_modules` or `tests`). Defaults to `node_modules`."
                <> value "node_modules"
            )
        <*> strArgument
            ( metavar "MAIN_PATH"
                <> help "Path to the main Elm module to compile."
            )


opts :: ParserInfo Flags
opts =
    info
        (flags <**> helper)
        ( fullDesc
            <> header "elmid - like ghcid, but for Elm!"
            <> progDesc "Watches the filesystem and runs the Elm compiler automatically, giving you a collapsible list of errors"
        )
