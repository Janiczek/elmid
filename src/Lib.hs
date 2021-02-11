module Lib (Msg(..), initModel, app) where

import Errors (ErrorInfo(..))
import Brick
import Brick.AttrMap
import NriPrelude
import List
import Prelude (return, show, Show, Ord, Eq, error, String)
import System.Exit (ExitCode(ExitSuccess))
import qualified Graphics.Vty as V
import qualified Errors
import qualified Brick.Types as T
import qualified Data.List.NonEmpty as NonEmpty

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
    , appChooseCursor = showFirstCursor
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
drawAllGood = withAttr (attrName "good") <| str "All good!"

drawCompiling :: Maybe String -> Widget Name
drawCompiling triggerFile = 
  case triggerFile of
    Nothing -> str "Compiling"
    Just file ->
      hBox
        [ str "Compiling (triggered by: "
        , withAttr (attrName "path") <| str file
        , str ")"
        ]

drawErrors :: List Error -> Widget Name
drawErrors errors = 
  errors
  |> List.indexedMap drawError
  |> vBox

drawError :: Int -> Error -> Widget Name
drawError i err =
  if eExpanded err then
    drawExpandedError i <| eInfo err
  else
    drawCollapsedError i <| eInfo err

drawExpandedError :: Int -> ErrorInfo -> Widget Name
drawExpandedError i info =
  hBox
    [ clickable (ErrorAtIndex i) <| str "[-] "
    , vBox (firstLine : restOfLines)
    , str " "
    ]
      where
        firstLine = clickable (ErrorAtIndex i) <| str <| eHeaderLine info
        restOfLines = 
          eFullError info
              |> NonEmpty.toList
              |> (++ [" "])
              |> List.map (emptyLineToSpace >> str)

emptyLineToSpace :: String -> String
emptyLineToSpace line =
  if line == "" then
    " "
  else
    line

drawCollapsedError :: Int -> ErrorInfo -> Widget Name
drawCollapsedError i info =
  hBox
  -- TODO group by paths?
      [ clickable (ErrorAtIndex i) <| str "[+] "
      , clickable (ErrorAtIndex i) <| str <| eFirstLine info
      , clickable (ErrorAtIndex i) <| withAttr (attrName "path") <| str <| ePath info
      ]

------- ATTR MAP

attributeMap :: Model -> AttrMap
attributeMap _ = 
  attrMap V.defAttr 
    [ (attrName "good", fg V.green)
    , (attrName "path", fg V.brightBlack)
    ]

------- EVENT

handleEvent :: Model -> BrickEvent Name Msg -> EventM Name (Next Model)
handleEvent model event =
  case event of
    T.MouseDown (ErrorAtIndex toggledIndex) _ _ _ ->
      continue <|
        case mStatus model of
          Errors errors ->
            model { mStatus = 
                          errors
                            |> List.indexedMap (\i err -> 
                              if i == toggledIndex then err { eExpanded = not <| eExpanded err } 
                              else                      err
                            )
                            |> Errors
                  }
          _ -> model

    T.VtyEvent e ->
      case e of
        V.EvKey V.KEsc        []        -> halt model
        V.EvKey (V.KChar 'q') []        -> halt model
        V.EvKey (V.KChar 'c') [V.MCtrl] -> halt model
        _ -> continue model

    T.AppEvent e ->
      case e of
        RecompileStarted filepath ->
          continue <| model { mStatus = Compiling filepath }

        GotElmMakeOutput (exitCode, stdout, stderr) -> 
          continue <| model { mStatus = 
            if exitCode == ExitSuccess then
              AllGood
            else
              stderr
              |> Errors.fromElmMakeStderr
              |> List.map (\errorData -> Error errorData False)
              |> Errors
          }

    _ ->  continue model
