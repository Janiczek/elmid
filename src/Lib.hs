module Lib (Msg(..), initModel, app) where

import Errors (ErrorData(..))
import Brick
import Brick.AttrMap
import Cherry.Prelude
import List
import String
import Prelude (return, show, Show, Ord, Eq, error)
import System.Exit (ExitCode(ExitSuccess))
import qualified Graphics.Vty as V
import qualified Errors
import qualified Brick.Types as T

data Model = Model
  { mStatus :: Status
  , mLog :: List String
  }

data Status
  = AllGood
  | Compiling String
  | Errors (List Error)
  
data Error = Error
  { eData :: ErrorData
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
  = RecompileStarted String
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
drawAllGood = str "All good!"

drawCompiling :: String -> Widget Name
drawCompiling triggerFile = str <| String.toList <| "Compiling (triggered by: " ++ triggerFile ++ ")"

drawErrors :: List Error -> Widget Name
drawErrors errors = 
  errors
    |> List.indexedMap drawError
    |> vBox

drawError :: Int -> Error -> Widget Name
drawError i err =
  if eExpanded err then
    drawExpandedError i <| eData err
  else
    drawCollapsedError i <| eData err

drawExpandedError :: Int -> ErrorData -> Widget Name
drawExpandedError i (DummyError n) =
  hBox
    [ clickable (ErrorAtIndex i) <| str "[-] "
    , vBox
        [ clickable (ErrorAtIndex i) <| str "Dummy Error"
        , clickable (ErrorAtIndex i) <| str "==========="
        , str <| String.toList <| "  " ++ String.fromInt n
        , clickable (ErrorAtIndex i) <| str " "
        ]
    ]


drawCollapsedError :: Int -> ErrorData -> Widget Name
drawCollapsedError i (DummyError n) =
  clickable (ErrorAtIndex i) <|
    hBox
      [ str "[+] "
      , str <| String.toList <| "Dummy Error " ++ String.fromInt n
      ]

------- ATTR MAP

attributeMap :: Model -> AttrMap
attributeMap _ = attrMap V.defAttr []

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
              stdout
              |> Errors.fromElmMakeStdout
              |> List.map (\errorData -> Error errorData False)
              |> Errors
          }

    _ ->  continue model
