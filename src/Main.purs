module Main where

import Prelude

import Data.Lens (itraverseOf_)
import Data.Lens.Indexed (itraversed)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Document, Element)
import Web.DOM.Document (createElement, createTextNode, getElementsByTagName)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (appendChild)
import Web.DOM.Text as Text
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent)

foreign import insertAdjacentElementImpl :: EffectFn3 Element String Element Unit

insertAdjacentElement :: Element -> String -> Element -> Effect Unit
insertAdjacentElement = runEffectFn3 insertAdjacentElementImpl

effectDocument :: Effect Document
effectDocument = toDocument <$> (window >>= document)

handle :: Int -> Event -> Effect Unit
handle i e = case fromEvent e of
    Nothing -> pure unit
    Just keyboardEvent -> logShow i

insertDiv :: Int -> Element -> Effect Unit
insertDiv i h3 = do
    doc <- effectDocument
    div <- createElement "div" doc
    text <- createTextNode (show $ i + 1) doc
    _ <- appendChild (Text.toNode text) (Element.toNode div)
    _ <- setAttribute "style" "float:left;left:-5px;position:absolute;" div
    insertAdjacentElement h3 "beforebegin" div
    listener <- eventListener $ handle $ i + 1
    target <- toEventTarget <$> window
    addEventListener (EventType "keydown") listener false target

main :: Effect Unit
main = do
    doc <- effectDocument
    h3s <- getElementsByTagName "h3" doc >>= toArray
    itraverseOf_ itraversed insertDiv h3s
    logShow "hello"
