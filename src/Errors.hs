{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Errors (
    FileErrorGroup (..),
    Error (..),
    FormattedTextOptions (..),
    Message,
    MessageFragment (..),
    fromElmMakeStderr,
    firstLine,
    normalizeFragments,
) where

import Data.Aeson.Combinators.Decode (Decoder)
import qualified Data.Aeson.Combinators.Decode as JD
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified List
import qualified Maybe
import NriPrelude
import Prelude (Either, String, fromIntegral, lines, takeWhile, zip, (<$>))


data FileErrorGroup = FileErrorGroup
    { fPath :: String
    , fName :: String
    , fErrors :: List Error
    }


data Error = Error
    { eTitle :: String
    , eRegion :: Region
    , eMessage :: Message
    }


data Region = Region
    { rStart :: Position
    , rEnd :: Position
    }


data Position = Position
    { pLine :: Int
    , pColumn :: Int
    }


type Message = List MessageFragment


firstLine :: String -> Message -> String
firstLine fallback fragments =
    fragments
        |> List.head
        |> Maybe.map
            ( fragmentToString
                >> takeWhile (/= '\n')
            )
        |> Maybe.withDefault fallback


data MessageFragment
    = RawText String
    | FormattedText FormattedTextOptions
    | Newline
    deriving (Show)


fragmentToString :: MessageFragment -> String
fragmentToString fragment =
    case fragment of
        RawText text ->
            text
        FormattedText opts ->
            fString opts
        Newline ->
            ""


data FormattedTextOptions = FormattedTextOptions
    { fBold :: Bool
    , fUnderline :: Bool
    , fColor :: Maybe String
    , fString :: String
    }
    deriving (Show)


fromElmMakeStderr :: String -> Either String (List FileErrorGroup)
fromElmMakeStderr stderr =
    JD.eitherDecode errorsDecoder <| BS.pack stderr


errorsDecoder :: Decoder (List FileErrorGroup)
errorsDecoder =
    JD.key "errors" (JD.list fileErrorGroupDecoder)


fileErrorGroupDecoder :: Decoder FileErrorGroup
fileErrorGroupDecoder =
    FileErrorGroup
        <$> JD.key "path" JD.string
        <*> JD.key "name" JD.string
        <*> JD.key "problems" (JD.list errorDecoder)


errorDecoder :: Decoder Error
errorDecoder =
    Error
        <$> JD.key "title" JD.string
        <*> JD.key "region" regionDecoder
        <*> JD.key "message" messageDecoder


regionDecoder :: Decoder Region
regionDecoder =
    Region
        <$> JD.key "start" positionDecoder
        <*> JD.key "end" positionDecoder


positionDecoder :: Decoder Position
positionDecoder =
    Position
        <$> JD.key "line" (fromIntegral <$> JD.int)
        <*> JD.key "column" (fromIntegral <$> JD.int)


messageDecoder :: Decoder Message
messageDecoder =
    JD.list messageFragmentDecoder


messageFragmentDecoder :: Decoder MessageFragment
messageFragmentDecoder =
    JD.oneOf
        ( (RawText <$> JD.string)
            :| [FormattedText <$> formattedTextOptionsDecoder]
        )


normalizeFragments :: List MessageFragment -> List (List MessageFragment)
normalizeFragments fragments =
    fragments
        |> List.concatMap splitFragment
        |> joinToLines


joinToLines :: List MessageFragment -> List (List MessageFragment)
joinToLines fragments =
    go [] [] False <| zip fragments (List.drop 1 fragments)
  where
    go currentLine acc ignoreNext pairs =
        case pairs of
            [] -> List.reverse <| currentLine : acc
            (a, b) : rest ->
                let isLast = List.isEmpty rest
                    addedToLine =
                        if isLast
                            then b : a : currentLine
                            else a : currentLine
                    makeNewLine =
                        if ignoreNext
                            then ignore
                            else go [] (List.reverse addedToLine : acc) False rest
                    ignoreNextNewLine = go [] (List.reverse addedToLine : acc) True rest
                    continue = go addedToLine acc False rest
                    ignore = go currentLine acc False rest
                 in case (a, b) of
                        (RawText _, RawText _) -> makeNewLine
                        (Newline, RawText _) -> makeNewLine
                        (FormattedText _, Newline) -> ignoreNextNewLine
                        _ -> continue


splitFragment :: MessageFragment -> List MessageFragment
splitFragment fragment =
    case fragment of
        RawText text ->
            text
                |> lines
                |> List.map emptyLineToNewline
        FormattedText _ ->
            -- the assumption is that these never contain newlines
            [fragment]
        Newline ->
            [fragment]


emptyLineToNewline :: String -> MessageFragment
emptyLineToNewline line =
    if line == ""
        then Newline
        else RawText line


formattedTextOptionsDecoder :: Decoder FormattedTextOptions
formattedTextOptionsDecoder =
    FormattedTextOptions
        <$> JD.key "bold" JD.bool
        <*> JD.key "underline" JD.bool
        <*> JD.key "color" (JD.nullable JD.string)
        <*> JD.key "string" JD.string
