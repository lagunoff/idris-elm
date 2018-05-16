module Elm.Events

import Control.Pipeline
import IdrisScript
import IdrisScript.Objects
import Elm.Attributes
import Elm.Decode


-- Ported from https://github.com/elm-lang/html/blob/2.0.0/src/Html/Events.elm
%default total
%access public export

-- CUSTOM EVENTS

||| Options for an event listener. If `stopPropagation` is true, it means the
||| event stops traveling through the DOM so it will not trigger any other event
||| listeners. If `preventDefault` is true, any built-in browser behavior related
||| to the event is prevented. For example, this is used with touch events when you
||| want to treat them as gestures of your own, not as scrolls.
record Options where
  constructor MkOptions
  stopPropagation : Bool
  preventDefault : Bool
  
implementation ToJS Options (JSObject "Object") where
  toJS (MkOptions stopPropagation preventDefault) = unsafePerformIO $ io
  where
    io : JS_IO (JSValue (JSObject "Object"))
    io = do
      let arg1 = unpack $ toJS stopPropagation {to=JSBoolean}
      let arg2 = unpack $ toJS preventDefault {to=JSBoolean}
      ref <- jscall "({ stopPropagation: %0, preventDefault: %1 })" (Ptr -> Ptr -> JS_IO Ptr) arg1 arg2
      pure $ MkJSObject {con="Object"} ref


||| Everything is `False` by default.
|||     defaultOptions =
|||         { stopPropagation = False
|||         , preventDefault = False
|||         }
defaultOptions : Options
defaultOptions =
  MkOptions False False


||| Same as `on` but you can set a few options.
onWithOptions : String -> Options -> Decoder msg -> Attribute msg
onWithOptions name options decoder =
  MkAttribute
    $ unsafePerformIO
    $ jscall "A3(_elm_lang$virtual_dom$Native_VirtualDom.on, %0, %1, %2)"
      (String -> Ptr -> Ptr -> JS_IO Ptr)
      name (unpack $ toJS options {to=JSObject "Object"}) (believe_me decoder)


||| Create a custom event listener. Normally this will not be necessary, but
||| you have the power! Here is how `onClick` is defined for example:
|||     import Json.Decode as Json
|||     onClick : msg -> Attribute msg
|||     onClick message =
|||       on "click" (pure message)
||| The first argument is the event name in the same format as with JavaScript's
||| [`addEventListener`][aEL] function.
||| The second argument is a JSON decoder. Read more about these [here][decoder].
||| When an event occurs, the decoder tries to turn the event object into an Elm
||| value. If successful, the value is routed to your `update` function. In the
||| case of `onClick` we always just succeed with the given `message`.
||| If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
||| It really does help!
||| [aEL]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
||| [decoder]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
||| [tutorial]: https://github.com/evancz/elm-architecture-tutorial/
on : String -> Decoder msg -> Attribute msg
on name decoder =
  onWithOptions name defaultOptions decoder  


-- COMMON DECODERS


||| A `Decoder` for grabbing `event.target.value`. We use this to define
||| `onInput` as follows:
|||     import Json.Decode as Json
|||     onInput : (String -> msg) -> Attribute msg
|||     onInput tagger =
|||       on "input" (map tagger targetValue)
||| You probably will never need this, but hopefully it gives some insights into
||| how to make custom event handlers.
targetValue : Decoder String
targetValue =
   at ["target", "value"] string


||| A `Decoder` for grabbing `event.target.checked`. We use this to define
||| `onCheck` as follows:
|||     import Json.Decode as Json
|||     onCheck : (Bool -> msg) -> Attribute msg
|||     onCheck tagger =
|||       on "input" (map tagger targetChecked)
targetChecked : Decoder Bool
targetChecked =
   at ["target", "checked"] bool


||| A `Decoder` for grabbing `event.keyCode`. This helps you define
||| keyboard listeners like this:
|||     import Json.Decode as Json
|||     onKeyUp : (Int -> msg) -> Attribute msg
|||     onKeyUp tagger =
|||       on "keyup" (map tagger keyCode)
||| **Note:** It looks like the spec is moving away from `event.keyCode` and
||| towards `event.key`. Once this is supported in more browsers, we may add
||| helpers here for `onKeyUp`, `onKeyDown`, `onKeyPress`, etc.
keyCode : Decoder Int
keyCode =
   at ["keyCode"] int
  

-- MOUSE EVENTS


onClick : msg -> Attribute msg
onClick msg =
  on "click" (pure msg)


onDoubleClick : msg -> Attribute msg
onDoubleClick msg =
  on "dblclick" (pure msg)


onMouseDown : msg -> Attribute msg
onMouseDown msg =
  on "mousedown" (pure msg)


onMouseUp : msg -> Attribute msg
onMouseUp msg =
  on "mouseup" (pure msg)


onMouseEnter : msg -> Attribute msg
onMouseEnter msg =
  on "mouseenter" (pure msg)


onMouseLeave : msg -> Attribute msg
onMouseLeave msg =
  on "mouseleave" (pure msg)


onMouseOver : msg -> Attribute msg
onMouseOver msg =
  on "mouseover" (pure msg)


onMouseOut : msg -> Attribute msg
onMouseOut msg =
  on "mouseout" (pure msg)



-- FORM EVENTS


||| Capture [input](https://developer.mozilla.org/en-US/docs/Web/Events/input)
||| events for things like text fields or text areas.
||| It grabs the **string** value at `event.target.value`, so it will not work if
||| you need some other type of information. For example, if you want to track 
||| inputs on a range slider, make a custom handler with [`on`](#on).
||| For more details on how `onInput` works, check out [targetValue](#targetValue).
onInput : (String -> msg) -> Attribute msg
onInput tagger =
  on "input" (map tagger targetValue)


||| Capture [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)
||| events on checkboxes. It will grab the boolean value from `event.target.checked`
||| on any input event.
||| Check out [targetChecked](#targetChecked) for more details on how this works.
onCheck : (Bool -> msg) -> Attribute msg
onCheck tagger =
  on "change" (map tagger targetChecked)


onSubmitOptions : Options
onSubmitOptions =
  record { preventDefault = True } defaultOptions


||| Capture a [submit](https://developer.mozilla.org/en-US/docs/Web/Events/submit)
||| event with [`preventDefault`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault)
||| in order to prevent the form from changing the page’s location. If you need
||| different behavior, use `onWithOptions` to create a customized version of
||| `onSubmit`.
onSubmit : msg -> Attribute msg
onSubmit msg =
  onWithOptions "submit" onSubmitOptions (pure msg)


-- FOCUS EVENTS


onBlur : msg -> Attribute msg
onBlur msg =
  on "blur" (pure msg)


onFocus : msg -> Attribute msg
onFocus msg =
  on "focus" (pure msg)

