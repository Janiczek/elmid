module Errors (ErrorInfo (..), fromElmMakeStderr) where

import Data.List (isPrefixOf, lines)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split (keepDelimsL, split, whenElt)
import Data.Maybe (mapMaybe)
import qualified List
import qualified Maybe
import NriPrelude
import Prelude (String, dropWhile, show)


data ErrorInfo = ErrorInfo
    { ePath :: String
    , eFullError :: NonEmpty String
    , eHeaderLine :: String
    , eFirstLine :: String
    }


splitWhen :: (String -> Bool) -> List String -> List (List String)
splitWhen = split << keepDelimsL << whenElt


fromElmMakeStderr :: String -> List ErrorInfo
fromElmMakeStderr stderr =
    stderr
        -- TODO perhaps try changing \r or \r\n into \n?
        |> lines
        |> splitWhen isErrorHeader
        |> mapMaybe nonEmpty
        |> List.map toErrorInfo


isErrorHeader :: String -> Bool
isErrorHeader line =
    "-- " `isPrefixOf` line


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
        |> List.drop 3
        |> dropWhile (/= '-')
        |> dropWhile (== '-')
