module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstN lastN = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstN && entry.lastName == lastN

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy areEqual
  where
  areEqual :: Entry -> Entry -> Boolean
  areEqual a b = a.firstName == b.firstName && a.lastName == b.lastName
