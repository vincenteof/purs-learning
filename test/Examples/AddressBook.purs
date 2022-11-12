module Test.Examples.AddressBook
  ( addressBookMain
  ) where

import Prelude
import Examples.AddressBook (AddressBook, Entry, emptyBook, findEntry, insertEntry, findEntryByStreet, isInBook, removeDuplicates)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

john :: Entry
john =
  { firstName: "John"
  , lastName: "Smith"
  , address:
      { street: "123 Fake St.", city: "Faketown", state: "CA" }
  }

peggy :: Entry
peggy =
  { firstName: "Peggy"
  , lastName: "Hill"
  , address:
      { street: "84 Rainey St.", city: "Arlen", state: "TX" }
  }

ned :: Entry
ned =
  { firstName: "Ned"
  , lastName: "Flanders"
  , address:
      { street: "740 Evergreen Terrace", city: "Springfield", state: "USA" }
  }

book :: AddressBook
book =
  insertEntry john
    $ insertEntry peggy
    $ insertEntry ned
        emptyBook

otherJohn :: Entry
otherJohn =
  { firstName: "John"
  , lastName: "Smith"
  , address:
      { street: "678 Fake Rd.", city: "Fakeville", state: "NY" }
  }

bookWithDuplicate :: AddressBook
bookWithDuplicate =
  insertEntry john
    $ insertEntry otherJohn
        book

runChapterExamples :: TestSuite
runChapterExamples = do
  test "Todo for book maintainers - Add tests for chapter examples" do
    Assert.equal true true
  suite "findEntry" do
    test "Lookup existing"
      $ Assert.equal (Just ned)
      $ findEntry ned.firstName ned.lastName book
    test "Lookup missing"
      $ Assert.equal Nothing
      $ findEntry "unknown" "person" book

addressBookMain :: Effect Unit
addressBookMain =
  runTest do
    runChapterExamples
    {-  Move this block comment starting point to enable more tests
This line should have been automatically deleted by resetSolutions.sh. See Chapter 2 for instructions. -}
    suite "Exercise - findEntryByStreet" do
      test "Lookup existing" do
        Assert.equal (Just john)
          $ findEntryByStreet john.address.street book
      test "Lookup missing" do
        Assert.equal Nothing
          $ findEntryByStreet "456 Nothing St." book
    suite "Exercise - isInBook" do
      test "Check existing" do
        Assert.equal true
          $ isInBook ned.firstName ned.lastName book
      test "Check missing" do
        Assert.equal false
          $ isInBook "unknown" "person" book
    test "Exercise - removeDuplicates" do
      Assert.equal book
        $ removeDuplicates bookWithDuplicate
