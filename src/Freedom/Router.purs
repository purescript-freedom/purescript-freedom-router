module Freedom.Router
  ( router
  , link
  , navigateTo
  , redirectTo
  , goForward
  , goBack
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Freedom.Markup as H
import Freedom.Subscription (Subscription, subscription)
import Freedom.VNode (VObject)
import Web.Event.Event (EventType, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.History (DocumentTitle(..), URL(..), back, forward, pushState, replaceState)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (Window, history, location, toEventTarget)

router
  :: forall f state
   . Functor (f state)
  => (String -> FreeT (f state) Aff Unit)
  -> Subscription f state
router matcher =
  subscription \transform -> do
    let handler = do
          l <- window >>= location
          path <- (<>) <$> pathname l <*> search l
          launchAff_ $ transform $ matcher path
    handler
    listener <- eventListener $ const handler
    window <#> toEventTarget >>= addEventListener popstate listener false

link
  :: forall f state m
   . Functor (f state)
  => MonadRec m
  => MonadEffect m
  => String
  -> VObject f state m
link url = H.a # H.href url # H.onClick onClick
  where
    onClick evt = liftEffect do
      preventDefault evt
      navigateTo url

navigateTo :: String -> Effect Unit
navigateTo url = do
  window >>= history >>= pushState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate

redirectTo :: String -> Effect Unit
redirectTo url = do
  window >>= history >>= replaceState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate

goForward ::Effect Unit
goForward = window >>= history >>= forward

goBack :: Effect Unit
goBack = window >>= history >>= back

null :: Foreign
null = unsafeToForeign Nothing

foreign import dispatchEvent
  :: EventType
  -> Window
  -> Effect Unit
