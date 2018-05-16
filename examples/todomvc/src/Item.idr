module Item

import IdrisScript
import Elm.Html
import Elm.Attributes
import Elm.Events
import Elm.Cmd
import Elm.Decode
import Elm.Encode
import Utils


currentTarget : Decoder Json
currentTarget =
  at ["currentTarget"] Any

keyCodeEq : Int -> a -> Decoder a
keyCodeEq code action = do
  code' <- keyCode
  if code' == code then pure action else Failure "Different key code"
  

public export
record Model where
  constructor MkModel
  title : String
  completed : Bool
  editing : Maybe String

public export
data Action
  = Completed Bool
  | Destroy
  | EditingOn Json
  | EditInput String
  | EditingCancel
  | EditingCommit

export
init : String -> Model
init title =
  MkModel title False Nothing

export
update : Action -> Update Model Action ()
update action = case action of
  (Completed x) => modifyModel $ record { completed = x }
  Destroy => pure ()
  (EditingOn (MkJson el)) => do
    lift $ setTimeout 100 $ jscall "%0.parentElement.querySelector('.edit').focus()" (JSRef -> JS_IO ()) el
    modifyModel $ \model => record { editing = Just (title model) } model
  (EditInput x) => modifyModel $ record { editing $= map (const x) }
  EditingCancel => modifyModel $ record { editing = Nothing }
  EditingCommit => do 
    model <- getModel
    case editing model of
      (Just "") => batchCommand $ pure $ Just Destroy
      (Just str) => putModel $ record { editing = Nothing, title = str } model
      Nothing => pure ()
    
export
view : Model -> Html Action
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
    , onInput EditInput
    , onBlur EditingCommit
    , on "keypress" (keyCodeEq 13 EditingCommit) -- Enter
    , on "keydown" (keyCodeEq 27 EditingCancel) -- Escape
    ] []
  ]

