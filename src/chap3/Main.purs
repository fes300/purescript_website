module Main where

import Prelude (Unit, show, (<>))
import Euler (answer)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ("The Euler answer is " <> show answer)

