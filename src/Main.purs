module Main where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Document, Element)
import Web.DOM.Document (createElement, getElementsByTagName)
import Web.DOM.HTMLCollection (toArray)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

foreign import insertAdjacentElementImpl :: EffectFn3 Element String Element Unit

insertAdjacentElement :: Element -> String -> Element -> Effect Unit
insertAdjacentElement = runEffectFn3 insertAdjacentElementImpl

insertDiv :: Document -> Element -> Effect Unit
insertDiv document h3 = do
    div <- createElement "div" document
    insertAdjacentElement h3 "beforebegin" div

main :: Effect Unit
main = do
    doc <- toDocument <$> (window >>= document)
    h3s <- getElementsByTagName "h3" doc >>= toArray
    traverse_ (insertDiv doc) h3s 
    logShow "hello"
