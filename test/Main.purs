module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Examples.AddressBook (addressBookMain)

main :: Effect Unit
main = do
  addressBookMain
  log "🍝"
