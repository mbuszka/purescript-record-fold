module Test.Main where

import Prelude

import Data.Record.Fold (applyTo, collect, length, rEq, rMap, rShow)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = do
  log $ show $ length { a: 123, b: "hello"}
  log $ show $ rShow { a: 123, b: "hello"}
  log $ show $ rShow $ rMap (Just) { a: 123, b: "hello" }
  log $ show $ rShow $ applyTo 5 { a: \x -> x + 1, b: \y -> y - 2 }
  log $ show $ rEq { a: 8, b: "string", c: true} {a: 8, b: "string", c: true}
  log $ show $ rEq { a: 8, b: "string", c: true} {a: 9, b: "string", c: true}
  log $ show $ rEq { a: 8, b: "string", c: true} {a: 8, b: "tring", c: true}
  let x = collect { a: Just 5, b: Just "sth" }
  log $ show $ map rShow x
