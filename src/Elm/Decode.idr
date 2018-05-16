module Elm.Decode

import IdrisScript
import IdrisScript.Arrays
import IdrisScript.Objects


%default total
%access public export


data Json = MkJson JSRef


pack : JSRef -> Json
pack = MkJson

unpack : Json -> JSRef
unpack (MkJson ref) = ref

interpJSType : JSType -> Type
interpJSType ty = case ty of
  JSNumber => Double
  JSString => String
  JSBoolean => Bool
  JSFunction => ()
  JSNull => ()
  (JSObject _) => ()
  JSUndefined => ()

data Decoder : Type -> Type where
  Success : a -> Decoder a
  Failure : String -> Decoder a
  Prim : (ty : JSType) -> Decoder (interpJSType ty)
  Array : Decoder a -> Decoder (List a)
  At : List String -> Decoder a -> Decoder a
  Any : Decoder Json
  Alt : Decoder a -> Decoder b -> Decoder (Either a b)
  (>>=) : Decoder a -> (a -> Decoder b) -> Decoder b
  
implementation Functor Decoder where
  map f ma = ma >>= Success . f

implementation Applicative Decoder where
  pure = Success
  mf <*> ma = do pure (!mf !ma)
  
implementation Monad Decoder where
  (>>=) = Decode.(>>=)

      
runDecoder : Decoder a -> Json -> Either String a
runDecoder decoder json =
  run decoder $ snd jsValue
  where
    jsValue : (ty ** JSValue ty)
    jsValue =
       unsafePerformIO $ IdrisScript.pack $ unpack json

    foldArray : (acc -> JSRef -> acc) -> acc -> JSValue JSArray-> acc
    foldArray reducer initial array = 
      believe_me
        $ unsafePerformIO
        $ jscall
          "%0.reduce(function(acc, x) { return %1(acc)(x); }, %2)"
          (Ptr -> Ptr -> Ptr -> JS_IO Ptr)
          (unpack array) (believe_me reducer) (believe_me initial)
                    
    run : Decoder a -> JSValue c -> Either String a
    run (Success x) _ = Right x
    run (Failure x) _ = Left x
    run (x >>= f) val = run x val >>= \w => run (f w) val
    run (Prim JSNumber) val@(MkJSNumber _) = Right $ fromJS {to=Double} val
    run (Prim JSString) val@(MkJSString _) = Right $ fromJS {to=String} val
    run (Prim JSBoolean) val@(MkJSBoolean _) = Right $ fromJS {to=Bool} val
    run (Prim JSFunction) val@(MkJSFunction _) = Right ()
    run (Prim JSNull) val@(MkJSNull _) = Right ()
    run (Prim (JSObject x)) (MkJSObject c {con}) with (decEq x con)
      run (Prim (JSObject x)) (MkJSObject c {con}) | Yes p = Right ()
      run (Prim (JSObject x)) (MkJSObject c {con}) | No p = Left $ "Expected constructor " ++ x ++ ", got " ++ con
    run (Prim JSUndefined) val@(MkJSUndefined _) = Right ()
    run (Array d) val@(MkJSObject _ {con="Array"}) =
      foldArray (\acc, ref => go d acc (snd $ unsafePerformIO $ IdrisScript.pack ref)) (Right Nil) val
      where
        go : Decoder a -> Either String (List a) -> JSValue c -> Either String (List a)
        go d (Left e) v = Left e
        go d (Right xs) v = do x <- run d v; pure $ x::xs 
    run (At xs d) y =
      go d xs y
      where
        go : Decoder a -> List String -> JSValue c -> Either String a
        go d [] c = run d c
        go d (x::xs') (MkJSObject c {con}) with (unsafePerformIO $ getProperty x (MkJSObject c {con}))
          go d (x::xs') (MkJSObject c) | Just (t ** v) = go d xs' v
          go d (x::xs') (MkJSObject c) | Nothing = Left $ "key _." ++ (unwords $ intersperse "." xs) ++ " not found"
        go d _ _ = Left "Trying to access property of a non-object"
    run Any val = Right (pack . unpack $ val)
    run (Alt a b) val = case run a val of
      Right x => Right $ Left x
      Left _ => map Right $ run b val
    run _ y = Left "Invalid value"

float : Decoder Double
float =
  Prim JSNumber

array : Decoder a -> Decoder (List a)
array =
  Array

int : Decoder Int
int =
  map (cast {to=Int}) float
  
string : Decoder String
string =
  Prim JSString

bool : Decoder Bool
bool =
  Prim JSBoolean

null : Decoder ()
null =
  Prim JSNull

undefined : Decoder ()
undefined =
  Prim JSUndefined

at : List String -> Decoder a -> Decoder a
at =
  At

any : Decoder Json
any =
  Any

failure : String -> Decoder a 
failure =
  Failure


parse : String -> Either String Json
parse str =
  map MkJson
    $ believe_me {b=Either String JSRef}
    $ unsafePerformIO
    $ jscall
      """
      function(str, Left, Right) {
        try {
           return Right(JSON.parse(str));
        } catch (e) {
           return Left(e.message);
        }
      }(%0, %1, %2)
      """
      (String -> JSRef -> JSRef -> JS_IO JSRef)
      str (believe_me $ Left {a=String} {b=JSRef}) (believe_me $ Right {a=String} {b=JSRef})
