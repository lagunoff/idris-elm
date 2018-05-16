module Elm.Subs

import IdrisScript
import Elm.Types
import Elm.Attributes
import Elm.Decode
import Elm.Events
import Elm.Html


%default total


export
Subscribe' : FFI -> Type -> Type
Subscribe' f a = (a -> IO' f ()) -> IO' f (IO' f ())


export
Subscribe : Type -> Type
Subscribe = Subscribe' FFI_JS


export
subscribe : String -> Lazy (Subscribe a) -> Attribute a
subscribe name io =
  MkAttribute
    $ unsafePerformIO
    $ jscall "A2(_elm_lang$virtual_dom$Native_VirtualDom.subscribe, %0, %1)" (String -> Ptr -> JS_IO Ptr) name (believe_me io)


public export
data EventTarget = MkEventTarget JSRef


export  
window : EventTarget 
window =
  MkEventTarget $ unsafePerformIO $ jscall "window" (JS_IO Ptr)


export  
document : EventTarget 
document =
  MkEventTarget $ unsafePerformIO $ jscall "document" (JS_IO Ptr)


export  
body : EventTarget 
body =
  MkEventTarget $ unsafePerformIO $ jscall "document.body" (JS_IO Ptr)
  

export  
addEventListener : EventTarget -> String -> Decoder a -> Subscribe a
addEventListener (MkEventTarget target) name decoder =
  believe_me
    $ unsafePerformIO
    $ jscall
      """
      function (target, name, decoder, runDecoder) {
        return function (callback) {
          return function () {
            target.addEventListener(name, eventHandler);
            return function () {
              target.removeEventListener(name, eventHandler);
            };
            function eventHandler(value) {
              var ethr = runDecoder(decoder)(value);
              if (ethr.type === 1) callback(ethr.$1)();
            }
          };
        };
      }(%0, %1, %2, %3)
      """
      (Ptr -> String -> Ptr -> Ptr -> JS_IO Ptr)
      target name (believe_me decoder) (believe_me $ runDecoder {a=()})


export
keydown : (String -> a) -> Subscribe a
keydown message = 
  addEventListener window "keydown" (pure message <*> at ["key"] string)


export
mousemove : (Double -> Double -> a) -> Subscribe a
mousemove message  = 
  addEventListener window "mousemove" (decodeXY message)
  where
    decodeXY : (Double -> Double -> a) -> Decoder a
    decodeXY message =
      pure message <*> at ["clientX"] float <*> at ["clientY"] float
