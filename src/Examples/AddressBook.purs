module Examples.AddressBook
  ( Address
  , AddressBook
  , Entry
  , emptyBook
  , findEntry
  , findEntryByStreet
  , insertEntry
  , isInBook
  , removeDuplicates
  , showAddress
  , showEntry
  ) where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, any, nubByEq)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", "
  <> addr.city
  <> ", "
  <>
    addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", "
  <> entry.firstName
  <> ": "
  <>
    showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- \entry -> entry.address
-- 等价于 _.address

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- <<< 左 compose
-- >>> 右 compose
-- 基本上用 $ 就行

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry entry = entry.address.street == street

--- 这里写成 _.address.street >>> eq street 感觉没必要

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = any (\e -> e.firstName == firstName && e.lastName == lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq theSame
  where
  theSame e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName