module Main where

import Prelude (Unit, show, (<>))
import Euler (answer)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ("The answer is " <> show answer)
