module Home where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
import MyStore as MS
import Type.Prelude (Proxy(..))


type Input = Unit

type State = {x :: Int}

component
  :: forall q o m
   . MonadStore MS.Action MS.Store m
  => MonadEffect m
  => H.Component q Input o m
component = connect selectAll $
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  initialState {context} = context
  render _ =
    HH.div_ [ HH.text "Logged in. Home component" ]