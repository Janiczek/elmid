module Lib (ui) where

import Brick
import Cherry.Prelude
import String
import List

data CompilationError
  = TodoAddSomeMoreDetails Int

compilationErrors :: List CompilationError
compilationErrors = 
  [ TodoAddSomeMoreDetails 42
  , TodoAddSomeMoreDetails (-1)
  ]

ui :: Widget ()
ui = 
  if List.isEmpty compilationErrors then
    goodUi
  else
    badUi

goodUi :: Widget ()
goodUi = str "All good!"

badUi :: Widget ()
badUi = 
  String.fromInt (List.length compilationErrors) ++ " errors :("
    |> String.toList
    |> str
