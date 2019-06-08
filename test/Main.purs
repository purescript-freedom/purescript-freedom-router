module Test.Main where

import Prelude

import Effect (Effect)
import Test.Router.Parser as Parser

main :: Effect Unit
main = Parser.test
