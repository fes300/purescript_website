module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..))
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
showEntry x = x.firstName <> ", " <> x.lastName <> ": " <> showAddress x.address

showAddress :: Address -> String
showAddress y = y.street <> "," <> y.city <> "," <> y.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book
