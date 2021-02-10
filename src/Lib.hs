module Lib (initModel, app) where

import Brick
import Brick.AttrMap
import qualified Brick.Types as T
import qualified Debug
import Cherry.Prelude
import List
import String
import qualified Graphics.Vty as V
import Prelude (return, show, Show, Ord, Eq, error)

data Model = Model
  { mErrors :: List Error
  }

data Error = Error
  { eData :: ErrorData
  , eExpanded :: Bool
  }

data ErrorData
  = DummyError Int

initModel :: Model
initModel =
  Model { mErrors = errors }
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

app :: App Model () Name
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
  in
  if List.isEmpty errors then
    [drawGoodUi]
  else
    [drawBadUi errors]

drawGoodUi :: Widget Name
drawGoodUi = str "All good!"

drawBadUi :: List Error -> Widget Name
drawBadUi errors = 
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

handleEvent :: Model -> BrickEvent Name () -> EventM Name (Next Model)

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
    _ -> case Debug.log "vty event" e of _ -> continue model

handleEvent model _ = continue model
