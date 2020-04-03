module Freedom.Router
  ( router
  , link
  , navigateTo
  , redirectTo
  , goForward
  , goBack
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Freedom.Markup as H
import Freedom.Store (Query)
import Freedom.UI (VNode, Subscription)
import Web.Event.Event (EventType, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.History (DocumentTitle(..), URL(..), back, forward, pushState, replaceState)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (Window, history, location, toEventTarget)

router
  :: forall state
   . (String -> Query state -> Effect Unit)
  -> Subscription state
router effect query = do
  effectByPath
  listener <- eventListener $ const effectByPath
  window <#> toEventTarget >>= addEventListener popstate listener false
  where
    effectByPath = do
      l <- window >>= location
      path <- (<>) <$> pathname l <*> search l
      effect path query

link :: forall state. String -> VNode state
link url = H.a # H.href url # H.onClick onClick
  where
    onClick evt = const $ preventDefault evt *> navigateTo url

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
