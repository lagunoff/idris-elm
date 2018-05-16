module Elm.Http

import Elm.Cmd
import IdrisScript

%default total
%access public export


data Method = GET | POST | PUT | DELETE | PATCH


implementation Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show PATCH = "PATCH"


record Request where
  constructor MkRequest
  url : String
  method : Method
  body : Maybe String
  headers : List (String, String)
  withCredentials : Bool
  timeout : Maybe Nat


record Response where
  constructor MkResponse
  url : String
  status : Nat
  statusText : String
  headers : List (String, String)
  body : String


data RawError 
  = BadUrl String
  | Timeout
  | NetworkError


send : Request -> Command (Either RawError Response)
send (MkRequest url method body headers withCredentials timeout) =
  Subscribe $ believe_me $ unsafePerformIO $ jscall
  """
  function (callback) {
    return function () {
      var xhr = new XMLHttpRequest();
      xhr.addEventListener('error', function () {
        callback(%0.More(%0.NetworkError))();
        callback(%0.Done)();
      });
      xhr.addEventListener('timeout', function () {
        callback(%0.More(%0.Timeout))();
        callback(%0.Done)();
      });
      xhr.addEventListener('load', function () {
        var response = %0.Success(xhr.responseURL)(%0.toNat(xhr.status))(xhr.statusText)(%0.parseHeaders(xhr.getAllResponseHeaders()))(xhr.response || xhr.responseText);
        callback(%0.More(response))();
        callback(%0.Done)();
      });
      if (%0.timeout) xhr.timeout = %0.timeout;
      if (typeof (%0.withCredentials) !== 'undefined') xhr.withCredentials = %0.withCredentials;
      try {
        xhr.open(%0.method, %0.url, true);
      } catch (e) {
         callback(%0.More(%0.BadUrl(e.message)))();
         callback(%0.Done)();
      }
      %0.traverseHeaders(function (key) {
        return function (value) {
          return function () {
            xhr.setRequestHeader(key, value);
          };
        };
      })();
      xhr.send(%0.body);
      return function() { xhr.abort(); };
    };
  }
  """
  (JSRef -> JS_IO JSRef)
  args
  
  where
  success : String -> Nat -> String -> List (String, String) -> String -> Either () Response
  success a b c d e = Right {a=()} $ MkResponse a b c d e
  
  packArgs : JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> JSRef
  packArgs a b c d e =
    unsafePerformIO $ jscall "[%0, %1, %2, %3, %4]" (JSRef -> JSRef -> JSRef -> JSRef -> JSRef -> JS_IO JSRef) a b c d e
  
  prepareBody : Maybe String -> JSRef
  prepareBody Nothing = unsafePerformIO $ jscall "void 0" (JS_IO JSRef)
  prepareBody (Just x) = believe_me x

  prepareTimeout : Maybe Nat -> JSRef
  prepareTimeout Nothing = unsafePerformIO $ jscall "void 0" (JS_IO JSRef)
  prepareTimeout (Just x) = unpack $ toJS {to=JSNumber} $ cast {to=Int} x
  
  traverseHeaders : (String -> String -> JS_IO ()) -> JS_IO ()
  traverseHeaders cb =
    traverse_ {b=()} (\(k, v) => cb k v) headers
    
  parseHeaders : String -> List (String, String)
  parseHeaders x = []
    
  toNat : JSRef -> Nat
  toNat ref =
    case unsafePerformIO $ pack ref of
      (JSNumber ** value) => fromIntegerNat $ cast {to=Integer} $ fromJS {to=Int} value
      _ => assert_unreachable
  
  prepareArgs : JS_IO Ptr
  prepareArgs = do
    obj <- jscall "{}" (JS_IO Ptr)
    jscall "%0.url = %1" (Ptr -> String -> JS_IO ()) obj url
    jscall "%0.method = %1" (Ptr -> String -> JS_IO ()) obj (show method)
    jscall "%0.body = %1" (Ptr -> Ptr -> JS_IO ()) obj (prepareBody body)
    jscall "%0.withCredentials = %1" (Ptr -> Ptr -> JS_IO ()) obj (unpack $ toJS withCredentials {to=JSBoolean})
    jscall "%0.timeout = %1" (Ptr -> Ptr -> JS_IO ()) obj (prepareTimeout timeout)
    jscall "%0.traverseHeaders = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me traverseHeaders)
    jscall "%0.toNat = %1" (Ptr -> String -> JS_IO ()) obj (believe_me toNat)
    jscall "%0.parseHeaders = %1" (Ptr -> String -> JS_IO ()) obj (believe_me parseHeaders)
    jscall "%0.Done = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ Done {a=()})
    jscall "%0.More = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ More {a=()})
    jscall "%0.Success = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ success)
    jscall "%0.BadUrl = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ Left {b=()} . BadUrl)
    jscall "%0.Timeout = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ Left {b=()} Timeout)
    jscall "%0.NetworkError = %1" (Ptr -> Ptr -> JS_IO ()) obj (believe_me $ Left {b=()} NetworkError)
    pure obj  
    
  args : Ptr
  args = unsafePerformIO prepareArgs
