module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Number (sqrt)

main :: Effect Unit
main = do
  log "🍝"
  logShow $ diagonal 3.0 4.0

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)
