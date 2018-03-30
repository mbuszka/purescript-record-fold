module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Catamorphism

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ show $ recordLen { a: 123, b: "hello"}
  log $ show $ recordShow { a: 123, b: "hello"}
