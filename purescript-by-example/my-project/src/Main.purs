module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Math (sqrt)

main = do
  --log "Hello sailor!"
  logShow $ diagonal 3.0 4.0

diagonal w h = sqrt (w * w + h * h)
