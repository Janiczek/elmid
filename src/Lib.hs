module Lib (Msg (..), initModel, app) where

import Brick (App (..), AttrMap, BrickEvent, EventM, Next, Widget)
import qualified Brick as B
import qualified Brick.Types as BT
import Data.Function (on)
import qualified Data.List as List (groupBy)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Errors (ErrorInfo (..))
import qualified Errors
import qualified Graphics.Vty as V
import qualified List
import NriPrelude
import System.Exit (ExitCode (ExitSuccess))
import Prelude (String, return)


data Model = Model
    { mStatus :: Status
    , mLog :: List String
    }


data Status
    = AllGood
    | Compiling (Maybe String)
    | Errors
        (Set String) -- expanded files
        (Set ErrorInfo)
        (List ErrorInfo)


initModel :: Model
initModel =
    Model
        { mStatus = AllGood
        , mLog = []
        }


data Name
    = FileToToggle String
    | ErrorToToggle ErrorInfo
    | AppViewport
    deriving (Show, Ord, Eq)


data Msg
    = RecompileStarted (Maybe String)
    | GotElmMakeOutput ExitCode String
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
    let widget =
            case mStatus model of
                AllGood -> drawAllGood
                Compiling triggerFile -> drawCompiling triggerFile
                Errors expandedFiles expandedErrors errors -> drawErrors expandedFiles expandedErrors errors
     in [B.viewport AppViewport B.Vertical widget]


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


drawErrors :: Set String -> Set ErrorInfo -> List ErrorInfo -> Widget Name
drawErrors expandedFiles expandedErrors errors =
    errors
        |> List.groupBy ((==) `on` ePath)
        |> List.map (drawErrorsForPath expandedFiles expandedErrors)
        |> B.vBox


drawErrorsForPath :: Set String -> Set ErrorInfo -> List ErrorInfo -> Widget Name
drawErrorsForPath expandedFiles expandedErrors errors =
    let path =
            List.head errors
                |> fromJust
                |> ePath
        isExpanded = Set.member path expandedFiles

        button :: String
        button = if isExpanded then "(-) " else "(+) "
     in ( B.hBox
            [ (B.clickable (FileToToggle path) <| B.withAttr (B.attrName "path") <| B.str button)
            , (B.clickable (FileToToggle path) <| B.withAttr (B.attrName "path") <| B.str path)
            ] :
          if isExpanded
            then
                List.map (drawError expandedErrors) errors
                    ++ [B.str " "]
            else []
        )
            |> B.vBox


drawError :: Set ErrorInfo -> ErrorInfo -> Widget Name
drawError expandedErrors err =
    if isExpanded
        then drawExpandedError err
        else drawCollapsedError err
  where
    isExpanded = Set.member err expandedErrors


drawExpandedError :: ErrorInfo -> Widget Name
drawExpandedError err =
    B.hBox
        [ B.clickable event <| B.str "[-] "
        , B.vBox (firstLine : restOfLines)
        , B.str " "
        ]
  where
    event = ErrorToToggle err
    firstLine = B.clickable event <| B.str <| eHeaderLine err

    restOfLines :: List (Widget Name)
    restOfLines =
        eFullError err
            |> NonEmpty.toList
            |> (++ [" "])
            |> List.map (emptyLineToSpace >> B.str)


emptyLineToSpace :: String -> String
emptyLineToSpace line =
    if line == ""
        then " "
        else line


drawCollapsedError :: ErrorInfo -> Widget Name
drawCollapsedError err =
    B.hBox
        [ B.clickable event <| B.str "[+] "
        , B.clickable event <| B.str <| eFirstLine err
        ]
  where
    event = ErrorToToggle err


------- ATTR MAP

attributeMap :: Model -> AttrMap
attributeMap _ =
    B.attrMap
        V.defAttr
        [ (B.attrName "good", B.fg V.green)
        , (B.attrName "path", B.fg V.cyan)
        ]


------- EVENT

handleEvent :: Model -> BrickEvent Name Msg -> EventM Name (Next Model)
handleEvent model event =
    case event of
        BT.MouseDown name button _ _ ->
            case (name, button) of
                (_, V.BScrollUp) -> do
                    B.vScrollBy (B.viewportScroll AppViewport) (-3)
                    B.continue model
                (_, V.BScrollDown) -> do
                    B.vScrollBy (B.viewportScroll AppViewport) 3
                    B.continue model
                (ErrorToToggle err, _) ->
                    B.continue
                        <| case mStatus model of
                            Errors expandedPaths expandedErrors errors ->
                                model
                                    { mStatus =
                                        Errors
                                            expandedPaths
                                            (toggle err expandedErrors)
                                            errors
                                    }
                            _ -> model
                (FileToToggle path, _) ->
                    B.continue
                        <| case mStatus model of
                            Errors expandedPaths expandedErrors errors ->
                                model
                                    { mStatus =
                                        Errors
                                            (toggle path expandedPaths)
                                            expandedErrors
                                            errors
                                    }
                            _ -> model
                _ -> B.continue model
        BT.VtyEvent ve ->
            case ve of
                V.EvKey V.KEsc [] -> B.halt model
                V.EvKey (V.KChar 'q') [] -> B.halt model
                V.EvKey (V.KChar 'c') [V.MCtrl] -> B.halt model
                _ -> B.continue model
        BT.AppEvent ve ->
            case ve of
                RecompileStarted filepath ->
                    B.continue <| model{mStatus = Compiling filepath}
                GotElmMakeOutput exitCode stderr ->
                    B.continue
                        <| model
                            { mStatus =
                                if exitCode == ExitSuccess
                                    then AllGood
                                    else
                                        let errors = Errors.fromElmMakeStderr stderr
                                            expandedFiles =
                                                errors
                                                    |> List.map ePath
                                                    |> Set.fromList
                                         in Errors expandedFiles Set.empty errors
                            }
        _ -> B.continue model


toggle :: Ord a => a -> Set a -> Set a
toggle a set =
    if Set.member a set
        then Set.delete a set
        else Set.insert a set
