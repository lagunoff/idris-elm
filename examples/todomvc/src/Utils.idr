module Utils


import IdrisScript
import Elm.Decode
import Elm.Subs


%default total

export
setTimeout : Nat -> JS_IO a -> JS_IO ()
setTimeout delay io = do
  jscall
     "setTimeout(%1, %0)"
     (JSRef -> JSRef -> JS_IO ())
     (unpack $ toJS {to=JSNumber} $ cast {to=Int} delay) (believe_me io)
     
     
public export
data ReadError
  = NoItem
  | InvalidJson String
  | DecodeError String


public export
data WriteError
  = NoSpaceLeft


export
localStorageGetItem : String -> Decoder a -> JS_IO (Either ReadError a)
localStorageGetItem name decoder =
  read >>= \ethr =>
    case ethr of
      Right str => case parse str of
        Left e => pure $ Left $ InvalidJson e
        Right json => pure $ mapLeft DecodeError $ runDecoder decoder json
      Left e => pure $ Left e
where
  read : JS_IO (Either ReadError String)
  read = believe_me $
    jscall
      """
      function(name, NoItem, Right) {
         var item = localStorage.getItem(name);
         return item === null ? NoItem : Right(item);
      }(%0, %1, %2)
      """
      (String -> JSRef -> JSRef -> JS_IO JSRef)
      name (believe_me $ Left NoItem {b=String}) (believe_me $ Right {a=ReadError} {b=String})
  
  mapLeft : (a -> b) -> Either a c -> Either b c
  mapLeft proj (Left a) = Left $ proj a
  mapLeft proj (Right c) = Right c
  

export
localStorageSetItem : String -> Json -> JS_IO (Either WriteError ())
localStorageSetItem name (MkJson ref) = believe_me $
  jscall
    """
    function(name, item, NoSpaceLeft, Right) {
       try {
         localStorage.setItem(name, JSON.stringify(item));
         return Right;
       } catch (e) {
         return NoSpaceLeft;
       }
    }(%0, %1, %2, %3)
    """
    (String -> JSRef -> JSRef -> JSRef -> JS_IO JSRef)
    name ref (believe_me $ Left NoSpaceLeft {b=()}) (believe_me $ Right () {a=WriteError} )
  
  
export
pluralize : Nat -> String -> String -> String
pluralize (S Z) singular plural = singular
pluralize n singular plural = plural
  
  
-- https://gist.github.com/kaizhu256/4482069
export
genUUID4 : JS_IO String
genUUID4 =
  jscall
  """
  function() {
    var uuid = '', ii;
    for (ii = 0; ii < 32; ii += 1) {
      switch (ii) {
      case 8:
      case 20:
        uuid += '-';
        uuid += (Math.random() * 16 | 0).toString(16);
        break;
      case 12:
        uuid += '-';
        uuid += '4';
        break;
      case 16:
        uuid += '-';
        uuid += (Math.random() * 4 | 8).toString(16);
        break;
      default:
        uuid += (Math.random() * 16 | 0).toString(16);
      }
    }
    return uuid;
  }()
  """
  (JS_IO String)
  

export
readHash : JS_IO String
readHash =
  jscall "window.location.hash" (JS_IO String)


export
fixHash : String -> JS_IO ()
fixHash =
  jscall "window.location.hash = %0" (String -> JS_IO ())
  

export
hashChange : (String -> a) -> Subscribe a
hashChange proj = 
  believe_me
    $ unsafePerformIO
    $ jscall 
      """
      function(proj) {
        return function(callback) {
          return function () {
            var prev = window.onpopState;
            window.onpopstate = function() {
              callback(proj(window.location.hash))();
            };
            return function () {
              window.onpopstate = prev;  
            };
          };
        };
      }(%0)
      """
      (Ptr -> JS_IO Ptr)
      (believe_me proj)
