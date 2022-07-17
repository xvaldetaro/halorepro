module Parent (component) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Store.Monad (runStoreT)
import Halogen.Subscription as HS
import LoggedIn as LoggedIn
import LoggedOff as LoggedOff
import MyStore as MS
import Type.Proxy (Proxy(..))

type Slots =
  ( loggedOff :: H.Slot Query LoggedOff.Output Unit
  , loggedIn :: H.Slot Query Void Unit
  )

_loggedIn = Proxy :: Proxy "loggedIn"
_loggedOff = Proxy :: Proxy "loggedOff"

type Query :: ∀ k. k -> Type
type Query = Const Void

type State =
  { loggedInComponent :: Maybe (H.Component Query Unit Void Aff)
  , logoffSubId :: Maybe SubscriptionId
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
  initialState _ = { loggedInComponent: Nothing, logoffSubId: Nothing }

  render { loggedInComponent } =
    case loggedInComponent of
      Nothing ->
        HH.div [ HE.onClick $ const Login ] [ HH.text "Logged off. Tap to log in."]
      Just c -> HH.slot_ _loggedIn unit c unit

  handleAction = case _ of
    Login -> do
      { emitter, listener } <- H.liftEffect HS.create
      subId <- H.subscribe emitter

      let doLogoff = HS.notify listener Logoff

      loggedInComponent <- H.liftAff
        $ runStoreT (MS.initialStore doLogoff) MS.reduce LoggedIn.component
      H.modify_ _ { loggedInComponent = Just loggedInComponent, logoffSubId = Just subId }
    Logoff -> do
      H.liftEffect $ log "Logging off"
      {logoffSubId} <- H.get
      maybe (pure unit) H.unsubscribe logoffSubId
      H.modify_ _ { loggedInComponent = Nothing }
