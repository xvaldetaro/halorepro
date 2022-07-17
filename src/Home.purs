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

data Action = Logoff | Receive (Connected MS.Store Input)

type State = {logoff :: Effect Unit}

component
  :: forall q o m
   . MonadStore MS.Action MS.Store m
  => MonadEffect m
  => H.Component q Input o m
component = connect selectAll $
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }
  where
  initialState {context} = context
  handleAction = case _ of
    Logoff -> do
      {logoff} <- H.get
      H.liftEffect logoff
    Receive {context} -> H.put context
  render _ =
    HH.div [ HE.onClick $ const Logoff ] [ HH.text "Grandkid component: tap to logoff" ]