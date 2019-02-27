module Item

import IdrisScript
import Elm.Html
import Elm.Attributes
import Elm.Events
import Elm.Decode
import Elm.Encode
import Utils


currentTarget : Decoder Json
currentTarget =
  at ["currentTarget"] Any

keyCodeEq : Int -> a -> Decoder a
keyCodeEq code msg = do
  code' <- keyCode
  if code' == code then pure msg else Failure "Different key code"
  

public export
record Model where
  constructor MkModel
  title : String
  completed : Bool
  editing : Maybe String

public export
data Msg
  = Completed Bool
  | Destroy
  | EditingOn Json
  | Input String
  | Cancel
  | Commit

export
init : String -> Model
init title =
  MkModel title False Nothing

export
update : Msg -> Model -> JS_IO (Model, Maybe Msg)
update msg model = case msg of
  (Completed x) =>
    pure $ (record { completed = x } model, Nothing)
    
  (Destroy) =>
     pure (model, Nothing)
     
  (EditingOn (MkJson el)) => do
    setTimeout 100 $ jscall "%0.parentNode.querySelector('.edit').focus()" (JSRef -> JS_IO ()) el
    pure $ (record { editing = Just (title model) } model, Nothing)
    
  (Input x) =>
     pure $ (record { editing $= map (const x) } model, Nothing)
     
  (Cancel) =>
     pure $ (record { editing = Nothing } model, Nothing)
     
  (Commit) => 
    case editing model of
      (Just "") => pure (model, Just Destroy)
      (Just str) => pure $ (record { editing = Nothing, title = str } model, Nothing)
      Nothing => pure (model, Nothing)
    
export
view : Model -> Html Msg
view (MkModel title completed editing) =
  li
  [ classList 
    [ ("completed", completed)
    , ("editing", isJust editing)
    ]
  ]
  [ div
    [ class' "view"
    , on "dblclick" (map EditingOn currentTarget)
    ]
    [ input
      [ class' "toggle"
      , type "checkbox"
      , checked completed
      , onCheck Completed
      ] []
    , label_ [ text title ]
    , button [ class' "destroy", onClick Destroy ] []
    ]
  , input
    [ class' "edit"
    , value $ maybe "" id editing
    , onInput Input
    , onBlur Commit
    , on "keypress" (keyCodeEq 13 Commit) -- Enter
    , on "keydown" (keyCodeEq 27 Cancel) -- Escape
    ] []
  ]

