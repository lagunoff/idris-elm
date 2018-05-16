module Elm.Css

import Data.String.Views
import Control.Monad.State
import Control.Monad.Identity
import Control.Pipeline

%default total
%access public export


||| CSS units
data CssUnit = Em | Ex | Ch | Rem | Vw | Vh | Vmin | Vmax | Pct | Cm | Mm | In | Px | Pt | Pc

Show CssUnit where
  show Em = "em" 
  show Ex = "ex" 
  show Ch = "ch" 
  show Rem = "rem" 
  show Vw = "vw" 
  show Vh = "vh" 
  show Vmin = "vmin" 
  show Vmax = "vmax" 
  show Pct = "pct" 
  show Cm = "cm" 
  show Mm = "mm" 
  show In = "in" 
  show Px = "px" 
  show Pt = "pt" 
  show Pc = "pc"

||| CSS value
data CssValue : Type where
  MkKeyword : String -> CssValue
  MkUnit : Double -> CssUnit -> CssValue
  
||| Declaration
Declaration : Type
Declaration = (String, CssValue)

mutual
  ||| CSS rule
  data Rule : Type where
    MkRule : (glob : Bool) -> String -> List Mixin -> Rule
  
  ||| CSS mixin
  data Mixin : Type where
    MkRuleMixin : Rule -> Mixin
    MkDeclMixin : Declaration -> Mixin

||| Css
Css : Type -> Type
Css = State (List Mixin)

infix 2 :=
||| define declaration
(:=) : Cast from CssValue => String -> from -> Css ()
(:=) prop val =
  modify $ (::) $ MkDeclMixin (prop, cast {to=CssValue} val)


||| common rule
rule' : Bool -> String -> Css () -> Css ()
rule' global selector mixins = do
  let (_, nested) = runState mixins []
  modify $ (::) $ MkRuleMixin (MkRule global selector nested)

||| define local rule  
rule : String -> Css () -> Css ()
rule = rule' False

||| define global rule  
global_rule : String -> Css () -> Css ()
global_rule = rule' True
  
Show CssValue where
  show (MkKeyword kwd) = kwd
  show (MkUnit val unit) = show val ++ show unit

||| Cast Strings to CssValue
Cast String CssValue where
  cast = MkKeyword

||| Cast Double to CssValue
Cast Double CssValue where
  cast = flip MkUnit $ Px

||| Cast Int to CssValue
Cast Integer CssValue where
  cast val = cast {to=Double} val |> (flip MkUnit Px)

(Cast a CssValue, Cast b CssValue) => Cast (a, b) CssValue where
  cast (a, b) = MkKeyword $ show (cast {to=CssValue} a) ++ " " ++ show (cast {to=CssValue} b)


replaceAmp : String -> String -> String
replaceAmp parent child =
  case go (strList child) False [] of
    (True, xs) => pack xs |> reverse
    (False, _) => parent ++ " " ++ child
  where
    parent' : List Char
    parent' =
      reverse parent |> unpack
  
    go : StrList s -> Bool -> List Char -> (Bool, List Char)
    go SNil found xs = (found, xs)
    go (SCons '&' rest) found xs = go rest True (parent' ++ xs)
    go (SCons ch rest) found xs = go rest found (ch :: xs)


flattenMixins : List Mixin -> List (Bool, String, List Declaration)
flattenMixins = stepImpl Nothing
  where
    stepImpl : Maybe String -> List Mixin -> List (Bool, String, List Declaration)
    stepImpl Nothing (MkRuleMixin (MkRule glob selector mixins) :: xs) = stepImpl (Just selector) mixins ++ stepImpl Nothing xs
    stepImpl p@(Just parent) (MkRuleMixin (MkRule glob selector mixins) :: xs) = stepImpl (Just $ replaceAmp parent selector) mixins ++ stepImpl p xs
    stepImpl Nothing (MkDeclMixin _ :: xs) = stepImpl Nothing xs
    stepImpl p@(Just parent) ((MkDeclMixin decl) :: xs) = assert_total $ let (decls, rest) = helper [decl] xs in (True, parent, decls) :: stepImpl p rest
      where
        helper : List Declaration -> List Mixin -> (List Declaration, List Mixin)
        helper decls [] = (decls, [])
        helper decls (MkDeclMixin decl :: xs) = helper (decl :: decls) xs
        helper decls mixins@(MkRuleMixin _ :: xs) = (decls, mixins)
    stepImpl _ [] = []

        
||| pretty-print css declaration
pprintDeclaration : Declaration -> String
pprintDeclaration (prop, MkKeyword kwd) = prop ++ ":" ++ kwd
pprintDeclaration (prop, MkUnit val unit) = prop ++ ":" ++ show val ++ show unit

||| Pretty print Css
pprint : Css () -> String
pprint css =
  runState css []
    |> snd
    |> reverse
    |> flattenMixins
    |> map pprintRule
    |> intersperse "\n"
    |> foldl (++) ""
  where
    pprintRule : (Bool, String, List Declaration) -> String
    pprintRule (glob, selector, decls) =
      selector ++ " { " ++ (reverse decls |> map pprintDeclaration |> intersperse ";" |> foldl (++) "") ++ " } "

prepareCss : Css () -> Elab ()
prepareCss css = do
  fill (RConstant (Str $ pprint css))
  solve
