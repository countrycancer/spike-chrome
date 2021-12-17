module Main where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Element)
import Web.DOM.Document (createElement, getElementsByTagName)
import Web.DOM.HTMLCollection (toArray)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

foreign import insertAdjacentElementImpl :: EffectFn3 Element String Element Unit

insertAdjacentElement :: Element -> String -> Element -> Effect Unit
insertAdjacentElement = runEffectFn3 insertAdjacentElementImpl

main :: Effect Unit
main = do
    document <- window >>= document >>= toDocument >>> pure
    h3s <- getElementsByTagName "h3" document >>= toArray
    div <- createElement "div" document
    traverse_ (\h3 -> insertAdjacentElement h3 "beforebegin" div) h3s 
    logShow "hello"
