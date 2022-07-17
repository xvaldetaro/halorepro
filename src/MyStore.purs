module MyStore where

import Prelude

import Effect (Effect)

type Store =
  { x :: Int
  }

data Action

reduce :: Store -> Action -> Store
reduce s _ = s

initialStore :: Int -> Store
initialStore x = { x }
