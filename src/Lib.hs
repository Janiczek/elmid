module Lib (Msg(..), initModel, app) where

import Brick
import Brick.AttrMap
import qualified Brick.Types as T
import qualified Debug
import Cherry.Prelude
import List
import String
import qualified Graphics.Vty as V
import Prelude (return, show, Show, Ord, Eq, error)
import System.Exit (ExitCode)

data Model = Model
  { mErrors :: List Error
  , mLog :: List String
  }

data Error = Error
  { eData :: ErrorData
  , eExpanded :: Bool
  }

data ErrorData
  = DummyError Int

initModel :: Model
initModel =
  Model 
    { mErrors = errors 
    , mLog = [] 
    }
    where
      errors =
        [ Error 
          { eData = DummyError 42
          , eExpanded = True 
          }
        , Error 
          { eData = DummyError (-1)
          , eExpanded = False 
          }
        , Error 
          { eData = DummyError 999
          , eExpanded = True
          }
        ]

data Name = ErrorAtIndex Int 
  deriving (Show, Ord, Eq)

data Msg
  = GotElmMakeOutput (ExitCode, String, String)
  | Log String
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
  let
    errors = mErrors model
    log = mLog model
  in
  if List.isEmpty errors then
    [drawGoodUi log]
  else
    [drawBadUi log errors]

drawDebug :: List String -> Widget Name
drawDebug log =
  log
    |> List.map (String.toList >> str)
    |> vBox

drawGoodUi :: List String -> Widget Name
--drawGoodUi = str "All good!"
drawGoodUi log = hBox [str "All good!", drawDebug log]

drawBadUi :: List String -> List Error -> Widget Name
drawBadUi log errors = 
--  errors
--    |> List.indexedMap drawError
--    |> vBox
  hBox [
  errors
    |> List.indexedMap drawError
    |> vBox
       , drawDebug log
  ]

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

handleEvent model (T.MouseDown (ErrorAtIndex toggledIndex) _ _ _) = 
  continue (model { mErrors = 
    mErrors model
    |> List.indexedMap (\i err -> 
      if i == toggledIndex then
        err { eExpanded = not <| eExpanded err } 
      else
        err
    )})


handleEvent model (T.VtyEvent e) = 
  case e of
    V.EvKey V.KEsc [] -> halt model
    V.EvKey (V.KChar 'q') [] -> halt model
    V.EvKey (V.KChar 'c') [V.MCtrl] -> halt model
    _ -> continue <| addLog "vty event" (Debug.toString e) model

handleEvent model (T.AppEvent (GotElmMakeOutput output)) = 
  continue <| addLog "got elm output" (Debug.toString output) model

handleEvent model (T.AppEvent (Log message)) = 
  continue <| addLog "log" message model

handleEvent model e = 
  continue <| addLog "unhandled event" (Debug.toString e) model

addLog :: String -> String -> Model -> Model
addLog label message model =
  model { mLog = (label ++ ": " ++ message) : mLog model }
