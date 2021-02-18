module Errors (ErrorInfo (..), fromElmMakeStderr) where

import Data.List (isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split (keepDelimsL, split, whenElt)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified List
import qualified Maybe
import NriPrelude
import Text.PortableLines (lines)
import Prelude (String, dropWhile, reverse)


data ErrorInfo = ErrorInfo
    { ePath :: String
    , eFullError :: NonEmpty String
    , eHeaderLine :: String
    , eFirstLine :: String
    }
    deriving (Show, Ord, Eq)


splitWhen :: (String -> Bool) -> List String -> List (List String)
splitWhen = split << keepDelimsL << whenElt


fromElmMakeStderr :: String -> List ErrorInfo
fromElmMakeStderr stderr =
    stderr
        |> T.pack
        |> T.replace
            ("\r\n" ++ fileBoundaryLine ++ "\r\n")
            fileBoundaryLine
        |> T.unpack
        |> lines
        |> List.filter (not << isFileBoundaryLine)
        |> splitWhen isErrorHeader
        |> mapMaybe nonEmpty
        |> List.filter (not << isCompilingGroup)
        |> List.map toErrorInfo
  where
    fileBoundaryLine :: T.Text
    fileBoundaryLine = "====o======================================================================o===="
    fileBoundaryLine' = T.unpack fileBoundaryLine
    isFileBoundaryLine line =
        fileBoundaryLine' `isInfixOf` line


isCompilingGroup :: NonEmpty String -> Bool
isCompilingGroup errorLines =
    errorLines
        |> NonEmpty.head
        |> ("Compiling ..." `isPrefixOf`)


isErrorHeader :: String -> Bool
isErrorHeader line =
    "\ESC[36m-- " `isPrefixOf` line


toErrorInfo :: NonEmpty String -> ErrorInfo
toErrorInfo errorLines =
    ErrorInfo
        { ePath = errorPath errorLines
        , eFullError =
            errorLines
                |> NonEmpty.tail
                |> NonEmpty.fromList
        , eHeaderLine = NonEmpty.head errorLines
        , eFirstLine =
            errorLines
                |> NonEmpty.drop 2
                |> List.head
                |> Maybe.withDefault ""
        }


errorPath :: NonEmpty String -> String
errorPath errorLines =
    NonEmpty.head errorLines
        |> List.drop 8 -- '\ESC[36m-- '
        |> dropWhile (/= '-')
        |> dropWhile (== '-')
        |> List.drop 1 -- ' '
        |> reverse
        |> List.drop 4 -- '\ESC[0m'
        |> reverse
