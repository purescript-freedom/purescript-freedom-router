module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.Router (link, router)
import Freedom.Router.Parser (end, int, lit, match, param)
import Freedom.UI (VNode, Subscription)

data Route
  = Home
  | User Int
  | Users String
  | NotFound

type State = Route

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState: Home
  , subscriptions: [ router' ]
  , view
  }

router' :: Subscription State
router' = router \url query -> query.reduce $ const $ route url
  where
    route url = fromMaybe NotFound $ match url $
      Home <$ end
      <|>
      Users <$> (lit "users" *> param "name") <* end
      <|>
      User <$> (lit "users" *> int) <* end

view :: State -> VNode State
view state =
  case state of
    Home ->
      H.keyed "home" $ H.div # H.kids
        [ H.h1 # H.kids [ H.t "Home" ]
        , link "/users/51" # H.kids [ H.t "To user 51" ]
        ]
    User i ->
      H.keyed "user" $ H.div # H.kids
        [ H.h1 # H.kids [ H.t $ "User " <> show i ]
        , link "/users?name=ichiro" # H.kids [ H.t "To ichiro" ]
        ]
    Users name ->
      H.keyed "users" $ H.div # H.kids
        [ H.h1 # H.kids [ H.t $ "I am " <> name ]
        , link "/not_found" # H.kids [ H.t "Somewhere" ]
        ]
    NotFound ->
      H.keyed "notFound" $ H.div # H.kids
        [ H.h1 # H.kids [ H.t "NotFound" ]
        , link "/" # H.kids [ H.t "To Home" ]
        ]
