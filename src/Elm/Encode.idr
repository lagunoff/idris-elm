module Elm.Encode

import IdrisScript
import Elm.Decode


%default total
%access public export


object : List (String, Json) -> Json
object xs =
  unsafePerformIO $ go xs
  where
    go : List (String, Json) -> JS_IO Json
    go [] = MkJson <$> jscall "{}" (JS_IO JSRef)
    go ((k, MkJson v)::xs) = do
      (MkJson ref) <- go xs
      jscall "%0[%1] = %2" (JSRef -> String -> JSRef -> JS_IO ()) ref k v
      pure (MkJson ref)

array : List Json -> Json
array xs =
  unsafePerformIO $ go xs
  where
    go : List Json -> JS_IO Json
    go [] = MkJson <$> jscall "[]" (JS_IO JSRef)
    go ((MkJson v)::xs) = do
      (MkJson ref) <- go xs
      jscall "%0.push(%1)" (JSRef -> JSRef -> JS_IO ()) ref v
      pure (MkJson ref)

bool : Bool -> Json
bool True = MkJson $ unsafePerformIO $ jscall "true" (JS_IO JSRef)
bool False = MkJson $ unsafePerformIO $ jscall "false" (JS_IO JSRef)

float : Double -> Json
float x =
  MkJson $ unpack $ toJS {to=JSNumber} x

int : Int -> Json
int x =
  MkJson $ unpack $ toJS {to=JSNumber} x
  
string : String -> Json
string x =
  MkJson $ believe_me x

null : Json
null =
  MkJson $ unsafePerformIO $ jscall "null" (JS_IO JSRef)

undefined : Json
undefined =
  MkJson $ unsafePerformIO $ jscall "undefined" (JS_IO JSRef)


stringify : Nat -> Json -> String
stringify indent (MkJson json) =
  unsafePerformIO $ jscall "JSON.stringify(%0, null, %1)" (JSRef -> JSRef -> JS_IO String) json indent'
  where
    indent' : JSRef
    indent' =
      unpack $ toJS {to=JSNumber} $ cast {to=Int} indent
