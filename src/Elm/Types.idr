module Elm.Types

import IdrisScript


%default total
%access public export


||| Wrapper for Elm lists
data ElmList : Type -> Type where
  MkElmList : (ref : JSRef) -> ElmList a
  
%used MkElmList ref


unpack : ElmList msg -> Ptr
unpack (MkElmList ptr) =
  ptr


implementation Functor ElmList where
  map f (MkElmList ref) = 
    MkElmList
      $ unsafePerformIO
      $ jscall
        "A2(_elm_lang$core$Native_List.map, %0, %1)"
        (JSRef -> JSRef -> JS_IO JSRef)
        (believe_me f) ref


-- Support for list syntax
Nil : ElmList a
Nil =
  MkElmList $ unsafePerformIO $ jscall "_elm_lang$core$Native_List.Nil" (JS_IO JSRef)

-- Support for list syntax
(::) : a -> ElmList a -> ElmList a
(::) x xs =
  MkElmList $ unsafePerformIO $ jscall "_elm_lang$core$Native_List.Cons(%0, %1)" (JSRef -> JSRef -> JS_IO JSRef) (believe_me x) (unpack xs)
  
    
implementation Cast (List a) (ElmList a) where
  cast [] = Nil
  cast (x::xs) = x :: cast xs



data ElmTuple : Type -> Type -> Type where
  MkElmTuple : (ref : JSRef) -> ElmTuple a b


%used MkElmTuple ref


infixl 5 :=
(:=) : a -> b -> ElmTuple a b
(:=) a' b' =
  MkElmTuple
    $ unsafePerformIO
    $ jscall
      "A2(_elm_lang$core$Native_Utils.Tuple2, %0, %1)"
      (Ptr -> Ptr -> JS_IO Ptr)
      (believe_me a') (believe_me b')


implementation Cast (a, b) (ElmTuple a b) where
  cast = uncurry (:=)
