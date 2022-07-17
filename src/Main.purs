module Main where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import MyStore as MS
import Router as Router
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Slots = (router :: H.Slot Router.Query Void Unit)
_router = Proxy :: Proxy "router"

type State =
  { routerComponent :: Maybe (H.Component Router.Query Unit Void Aff)
  }

data Action = Login | Logoff

component :: ∀ q i o m. MonadAff m => MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { routerComponent: Nothing }

  render { routerComponent } =
    case routerComponent of
      Nothing ->
        HH.div [ HE.onClick $ const Login ] [ HH.text "Logged off. Tap to log in."]
      Just c ->
        HH.div
          [ HE.onClick $ const Logoff ]
          [ HH.text "Tap to log off."
          , HH.slot_ _router unit c unit
          ]

  handleAction = case _ of
    Login -> do
      routerComponent <- H.liftAff $ runStoreT (MS.initialStore 1) MS.reduce Router.component
      H.modify_ _ { routerComponent = Just routerComponent }
    Logoff -> do
      H.modify_ _ { routerComponent = Nothing }
