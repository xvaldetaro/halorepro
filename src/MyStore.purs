module MyStore where

import Prelude

import Effect (Effect)

type Store =
  { logoff :: Effect Unit
  }

data Action

reduce :: Store -> Action -> Store
reduce s _ = s

initialStore :: Effect Unit -> Store
initialStore logoff = { logoff }
