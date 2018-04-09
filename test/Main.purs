module Test.Main where

import Prelude

import Data.Record.Fold (applyTo, collect, length, rMap, rShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ length { a: 123, b: "hello"}
  log $ show $ rShow { a: 123, b: "hello"}
  log $ show $ rShow $ rMap (Just) { a: 123, b: "hello" }
  log $ show $ rShow $ applyTo 5 { a: \x -> x + 1, b: \y -> y - 2 }
  let x = collect { a: Just 5, b: Just "sth" }
  log $ show $ map rShow x
