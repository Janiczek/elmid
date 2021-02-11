module Lib (Msg (..), initModel, app) where

import Brick (App (..), AttrMap, BrickEvent, EventM, Next, Widget)
import qualified Brick as B
import qualified Brick.Types as T
import qualified Data.List.NonEmpty as NonEmpty
import Errors (ErrorInfo (..))
import qualified Errors
import qualified Graphics.Vty as V
import qualified List
import NriPrelude
import System.Exit (ExitCode (ExitSuccess))
import Prelude (Eq, Ord, Show, String, error, return, show)


data Model = Model
    { mStatus :: Status
    , mLog :: List String
    }


data Status
    = AllGood
    | Compiling (Maybe String)
    | Errors (List Error)


data Error = Error
    { eInfo :: ErrorInfo
    , eExpanded :: Bool
    }


initModel :: Model
initModel =
    Model
        { mStatus = AllGood
        , mLog = []
        }


data Name = ErrorAtIndex Int
    deriving (Show, Ord, Eq)


data Msg
    = RecompileStarted (Maybe String)
    | GotElmMakeOutput (ExitCode, String, String)
    deriving (Show)


app :: App Model Msg Name
app =
    App
        { appDraw = draw
        , appChooseCursor = B.showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = attributeMap
        }


------ DRAW

draw :: Model -> List (Widget Name)
draw model =
    case mStatus model of
        AllGood -> [drawAllGood]
        Compiling triggerFile -> [drawCompiling triggerFile]
        Errors errors -> [drawErrors errors]


drawAllGood :: Widget Name
drawAllGood = B.withAttr (B.attrName "good") <| B.str "All good!"


drawCompiling :: Maybe String -> Widget Name
drawCompiling triggerFile =
    case triggerFile of
        Nothing -> B.str "Compiling"
        Just file ->
            B.hBox
                [ B.str "Compiling (triggered by: "
                , B.withAttr (B.attrName "path") <| B.str file
                , B.str ")"
                ]


drawErrors :: List Error -> Widget Name
drawErrors errors =
    errors
        |> List.indexedMap drawError
        |> B.vBox


drawError :: Int -> Error -> Widget Name
drawError i err =
    if eExpanded err
        then drawExpandedError i <| eInfo err
        else drawCollapsedError i <| eInfo err


drawExpandedError :: Int -> ErrorInfo -> Widget Name
drawExpandedError i info =
    B.hBox
        [ B.clickable (ErrorAtIndex i) <| B.str "[-] "
        , B.vBox (firstLine : restOfLines)
        , B.str " "
        ]
  where
    firstLine = B.clickable (ErrorAtIndex i) <| B.str <| eHeaderLine info
    restOfLines =
        eFullError info
            |> NonEmpty.toList
            |> (++ [" "])
            |> List.map (emptyLineToSpace >> B.str)


emptyLineToSpace :: String -> String
emptyLineToSpace line =
    if line == ""
        then " "
        else line


drawCollapsedError :: Int -> ErrorInfo -> Widget Name
drawCollapsedError i info =
    B.hBox
        -- TODO group by paths?
        [ B.clickable (ErrorAtIndex i) <| B.str "[+] "
        , B.clickable (ErrorAtIndex i) <| B.str <| eFirstLine info
        , B.clickable (ErrorAtIndex i) <| B.withAttr (B.attrName "path") <| B.str <| ePath info
        ]


------- ATTR MAP

attributeMap :: Model -> AttrMap
attributeMap _ =
    B.attrMap
        V.defAttr
        [ (B.attrName "good", B.fg V.green)
        , (B.attrName "path", B.fg V.brightBlack)
        ]


------- EVENT

handleEvent :: Model -> BrickEvent Name Msg -> EventM Name (Next Model)
handleEvent model event =
    case event of
        T.MouseDown (ErrorAtIndex toggledIndex) _ _ _ ->
            B.continue
                <| case mStatus model of
                    Errors errors ->
                        model
                            { mStatus =
                                errors
                                    |> List.indexedMap
                                        ( \i err ->
                                            if i == toggledIndex
                                                then err{eExpanded = not <| eExpanded err}
                                                else err
                                        )
                                    |> Errors
                            }
                    _ -> model
        T.VtyEvent e ->
            case e of
                V.EvKey V.KEsc [] -> B.halt model
                V.EvKey (V.KChar 'q') [] -> B.halt model
                V.EvKey (V.KChar 'c') [V.MCtrl] -> B.halt model
                _ -> B.continue model
        T.AppEvent e ->
            case e of
                RecompileStarted filepath ->
                    B.continue <| model{mStatus = Compiling filepath}
                GotElmMakeOutput (exitCode, stdout, stderr) ->
                    B.continue
                        <| model
                            { mStatus =
                                if exitCode == ExitSuccess
                                    then AllGood
                                    else
                                        stderr
                                            |> Errors.fromElmMakeStderr
                                            |> List.map (\errorData -> Error errorData False)
                                            |> Errors
                            }
        _ -> B.continue model
