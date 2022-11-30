module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Examples.AddressBook (addressBookMain)
import Test.Examples.Recursion (recursionMain)
import Test.Examples.PatternMatching (patternMatchingMain)

main :: Effect Unit
main = do
  addressBookMain
  recursionMain
  patternMatchingMain
  log "🍝"
