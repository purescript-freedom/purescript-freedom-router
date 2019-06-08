module Test.Router.Parser (test) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Freedom.Router.Parser (end, int, lit, match, param)
import Test.Assert (assert)

data Route
  = Home
  | User Int
  | Users String
  | NotFound

derive instance routeEq :: Eq Route

route :: String -> Route
route url = fromMaybe NotFound $ match url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "name") <* end
  <|>
  User <$> (lit "users" *> int) <* end

test :: Effect Unit
test = do
  assert $ route "" == Home
  assert $ route "/" == Home
  assert $ route "/users?name=oreshinya" == Users "oreshinya"
  assert $ route "/users/42" == User 42
  assert $ route "/projects" == NotFound
