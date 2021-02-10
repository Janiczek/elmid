module Errors (ErrorData(..), fromElmMakeStdout) where

import Cherry.Prelude

data ErrorData
  = DummyError Int

fromElmMakeStdout :: String -> List ErrorData
fromElmMakeStdout stdout =
  [DummyError 42]
