module Main where

import Prelude

import Data.Array (length)
import Effect (Effect)
import Effect.Console (logShow)
import Web.DOM.Document (getElementsByTagName)
import Web.DOM.HTMLCollection (toArray)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

main :: Effect Unit
main = window >>= document >>= toDocument >>> getElementsByTagName "h3" >>= toArray >>= logShow <<< length
