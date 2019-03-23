module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.Router (link, router)
import Freedom.Router.Parser (end, int, lit, match, param)
import Freedom.Subscription (Subscription)
import Freedom.TransformF.Simple (VQueryF, reduce, transformF)
import Freedom.VNode (VNode)

data Route
  = Home
  | User Int
  | Users String
  | NotFound

type State = Route

type Sub = Subscription VQueryF State

type Html = VNode VQueryF State

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState: Home
  , subscriptions: [ router' ]
  , transformF
  , view
  }

router' :: Sub
router' = router \url -> reduce $ const $ route url
  where
    route url = fromMaybe NotFound $ match url $
      Home <$ end
      <|>
      Users <$> (lit "users" *> param "name") <* end
      <|>
      User <$> (lit "users" *> int) <* end

view :: State -> Html
view state =
  case state of
    Home ->
      H.el $ H.div # H.kids
        [ H.el $ H.h1 # H.kids [ H.t "Home" ]
        , H.el $ link "/users/51" # H.kids [ H.t "To user 51" ]
        ]
    User i ->
      H.el $ H.div # H.kids
        [ H.el $ H.h1 # H.kids [ H.t $ "User " <> show i ]
        , H.el $ link "/users?name=ichiro" # H.kids [ H.t "To ichiro" ]
        ]
    Users name ->
      H.el $ H.div # H.kids
        [ H.el $ H.h1 # H.kids [ H.t $ "I am " <> name ]
        , H.el $ link "/not_found" # H.kids [ H.t "Somewhere" ]
        ]
    NotFound ->
      H.el $ H.div # H.kids
        [ H.el $ H.h1 # H.kids [ H.t "NotFound" ]
        , H.el $ link "/" # H.kids [ H.t "To Home" ]
        ]
