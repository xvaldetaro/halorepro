module LoggedIn where

import Prelude

import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
import Home as Home
import MyStore as MS
import Type.Prelude (Proxy(..))

type Query :: ∀ k. k -> Type
type Query = Const Void

type Slots = (home :: H.Slot Query Void Unit)
_home = Proxy :: Proxy "home"

component
  :: ∀ q i o m
   . MonadStore MS.Action MS.Store m
  => MonadEffect m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  initialState _ = unit
  render _ = HH.slot_ _home unit Home.component unit
