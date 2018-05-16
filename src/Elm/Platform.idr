module Elm.Platform

import IdrisScript
import Elm.Decode
import Elm.Html
import Elm.Attributes
import Elm.Subs
import Control.Monad.Identity
import Control.Monad.State
import Elm.Cmd


%default total
%access public export


initialize : JS_IO ()
initialize = do
  jscall "_idris$exports.either = %0" (JSRef -> JS_IO ()) (believe_me $ eagerEither {a=()} {b=()} {c=()})
  jscall "_idris$exports.runDecoder = %0" (JSRef -> JS_IO ()) (believe_me $ runDecoder {a=()})
  jscall "_idris$exports.mapDecoder = %0" (JSRef -> JS_IO ()) (believe_me $ mapDecoder {a=Void} {b=()})
  jscall "_idris$exports.forceLazy = %0" (JSRef -> JS_IO ()) (believe_me $ Force {t=LazyValue} {a=()})
  jscall
    """
    _elm_lang$core$Native_Platform.initialize = function(init, update, subscriptions, renderer) {
       var model = init.hasOwnProperty('_0') ? init._0 : init;
       function tagger(action) {
         var monad = update(action);
         var tuple = %0(monad)(model)();
         model = tuple.hasOwnProperty('$1') ? tuple.$1 : tuple;
         %1(tuple.$2)(function (msg) { return function () { tagger(msg); } })();
         stepper(model);
       }
       var stepper = renderer(tagger, model);
    }
    """
    (JSRef -> JSRef -> JS_IO ())
    (believe_me $ runUpdate {model=()} {msg=()}) (believe_me $ runCmd {msg=()})
where  
  mapDecoder : (a -> b) -> Decoder a -> Decoder b
  mapDecoder = map
  
  runUpdate : Update model msg () -> model -> JS_IO (UpdateState model msg)
  runUpdate m model =
    snd <$> runStateT m (MkUpdateState model Never)
  
  runCmd : Cmd msg -> (msg -> JS_IO ()) -> JS_IO ()
  runCmd command onMessage =
    flip (eval command) (pure ()) $ \maybeMsg => case maybeMsg of
      Nothing => pure ()
      Just msg => onMessage msg
      
  eagerEither : (a -> c) -> (b -> c) -> Either a b -> c
  eagerEither proj_a proj_b =
    either proj_a proj_b


record Program model action where
  constructor MkProgram
  init : model
  update : action -> Update model action ()
  view : model -> Html action


embed : JSRef -> Program model action -> JS_IO ()
embed elem (MkProgram init update view) = do
  setup <- makeSetup
  start setup
where
  makeSetup : JS_IO JSRef
  makeSetup =
    jscall
    """
    A2(_elm_lang$virtual_dom$Native_VirtualDom.program, null, { init: %0, update: %1, view: %2 })()
    """
    (Ptr -> Ptr -> Ptr -> JS_IO Ptr)
    (believe_me init) (believe_me update) (believe_me view)

  start : JSRef -> JS_IO ()
  start setup =
    jscall
    """
    function (elem, setup) { var Elm = {}; setup(Elm); Elm.embed(elem); }(%0, %1)
    """
    (Ptr -> Ptr -> JS_IO ()) elem setup


fullscreen : Program model action -> JS_IO ()
fullscreen program = do
  elem <- jscall "document.body" (JS_IO Ptr)
  embed elem program
