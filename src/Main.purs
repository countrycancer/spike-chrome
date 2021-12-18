module Main where

import Prelude

import Data.Array (filterA, take)
import Data.Lens (itraverseOf_)
import Data.Lens.Indexed (itraversed)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Document, Element)
import Web.DOM.Document (createElement, createTextNode, getElementsByTagName)
import Web.DOM.Element (fromNode, getAttribute, setAttribute)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (appendChild, parentNode)
import Web.DOM.Text as Text
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Location (setHref)
import Web.HTML.Window (document, location, toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

foreign import insertAdjacentElementImpl :: EffectFn3 Element String Element Unit

insertAdjacentElement :: Element -> String -> Element -> Effect Unit
insertAdjacentElement = runEffectFn3 insertAdjacentElementImpl

effectDocument :: Effect Document
effectDocument = toDocument <$> (window >>= document)

handle :: Int -> String -> Event -> Effect Unit
handle i href e = case fromEvent e of
    Nothing -> pure unit
    Just keyboardEvent -> if show i == key keyboardEvent 
        then do
            loc <- window >>= location
            setHref href loc
        else pure unit

effectHref :: Element -> Effect String
effectHref h3 = do
    maybeA <- parentNode (Element.toNode h3)
    case maybeA >>= fromNode of
        Nothing -> pure ""
        Just element -> fromMaybe "" <$> getAttribute "href" element

insertDiv :: Int -> Element -> Effect Unit
insertDiv i result = do
    doc <- effectDocument
    div <- createElement "div" doc
    text <- createTextNode (show $ i + 1) doc
    _ <- appendChild (Text.toNode text) (Element.toNode div)
    _ <- setAttribute "style" "left: -12px; position: absolute;" div
    insertAdjacentElement result "beforebegin" div
    href <- effectHref result
    listener <- eventListener $ handle (i + 1) href
    target <- toEventTarget <$> window
    addEventListener (EventType "keydown") listener false target

main :: Effect Unit
main = do
    doc <- effectDocument
    h3s <- getElementsByTagName "h3" doc >>= toArray
    results <- filterA (\h3 -> null >>> not <$> (effectHref h3)) $ h3s
    itraverseOf_ itraversed insertDiv $ take 9 results
    logShow "hello"
