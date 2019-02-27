module Elm.Platform
import IdrisScript
import Elm.Html
import Data.IORef


%default total
%access public export


Handler : Type -> Type
Handler msg = msg -> JS_IO ()


record Program (msg : Type) where
  constructor MkProgram
  vdom : IORef (Html msg)
  domNode : IORef Ptr
  handler : IORef (Handler msg)
  eventNode : Ptr
  

createProgram : Html msg -> Handler msg -> JS_IO (Program msg)
createProgram vdom handler = do
  vdom' <- newIORef' vdom
  handler' <- newIORef' handler
  eventNode <- jscall
    """
    function() {
      var tagger = function(msg) { %0.val(msg)(); };
      return { tagger: tagger, parent: undefined };
    } ()
    """
    (Ptr -> JS_IO Ptr)
    (believe_me handler')
  
  domNode <- jscall "_elm_lang$virtual_dom$Native_VirtualDom.render(%0, %1)" (Ptr -> Ptr -> JS_IO Ptr) (unpack vdom) eventNode
  domNode' <- newIORef' domNode
  pure $ MkProgram vdom' domNode' handler' eventNode


actuate : Program msg -> Html msg -> JS_IO ()
actuate (MkProgram vdom' domNode' handler' eventNode) nextVdom = do
  vdom <- readIORef' vdom'
  domNode <- readIORef' domNode'
  handler <- readIORef' handler'
  patches <- jscall "_elm_lang$virtual_dom$Native_VirtualDom.diff(%0, %1)" (Ptr -> Ptr -> JS_IO Ptr) (unpack vdom) (unpack nextVdom)
  jscall "_elm_lang$virtual_dom$Native_VirtualDom.applyPatches(%0, %1, %2, %3)" (Ptr -> Ptr -> Ptr -> Ptr -> JS_IO ()) domNode (unpack vdom) patches eventNode
  writeIORef' vdom' nextVdom


noopHandler : Handler msg
noopHandler = const (pure ())


attach : Program msg -> Ptr -> JS_IO ()
attach (MkProgram _ domNode _ _) parentNode = do
  domNodePtr <- readIORef' domNode
  jscall "%0.appendChild(%1)" (Ptr -> Ptr -> JS_IO ()) parentNode domNodePtr


detach : Program msg -> JS_IO ()
detach (MkProgram _ domNodeRef _ _) = do
  domNode <- readIORef' domNodeRef
  jscall "%0.parentNode && %0.parentNode.removeChildren(%0)" (Ptr -> JS_IO ()) domNode
  

replaceHandler : Program msg -> Handler msg -> JS_IO ()
replaceHandler (MkProgram _ _ handlerRef _) nextHandler = do
  writeIORef' handlerRef nextHandler


embed : Ptr -> Html msg -> Handler msg -> JS_IO (Program msg)
embed elem vdom handler = do
  inst <- createProgram vdom handler
  attach inst elem
  pure inst


fullscreen : Html msg -> Handler msg -> JS_IO (Program msg)
fullscreen vdom handler = do
  elem <- jscall "document.body" (JS_IO Ptr)
  embed elem vdom handler


embed' : Ptr -> model -> (model -> Html msg) -> (IORef model -> Program msg -> Handler msg) -> JS_IO (Program msg)
embed' elem init view eval = do
  modelRef <- newIORef' init
  inst <- createProgram (view init) noopHandler
  replaceHandler inst (eval modelRef inst)
  attach inst elem
  pure inst


fullscreen' : model -> (model -> Html msg) -> (IORef model -> Program msg -> Handler msg) -> JS_IO (Program msg)
fullscreen' init view eval = do
  elem <- jscall "document.body" (JS_IO Ptr)
  embed' elem init view eval
