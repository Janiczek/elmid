module Errors (ErrorInfo(..), fromElmMakeStderr) where

import NriPrelude
import Data.List (isPrefixOf, lines)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)
import Prelude (dropWhile, String)
import qualified List
import qualified Maybe
import qualified Data.List.NonEmpty as NonEmpty

data ErrorInfo = ErrorInfo
  { ePath :: String
  , eFullError :: NonEmpty String
  , eFirstLine :: String
  }

fromElmMakeStderr :: String -> List ErrorInfo
fromElmMakeStderr stderr =
  stderr
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
    , eFullError = errorLines
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
