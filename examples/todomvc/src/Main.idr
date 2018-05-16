module Main

import IdrisScript
import Elm.Html
import Elm.Attributes 
import Elm.Events
import Item as Item
import Elm.Cmd
import Elm.Subs
import Control.Pipeline
import Elm.Platform
import Elm.Decode
import Elm.Encode
import Utils


public export
data Filter = All | Active | Completed  


Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"


Eq Filter where
  All == All = True
  Active == Active = True
  Completed == Completed = True
  _ == _ = False
  
  
applyFilter : Filter -> Item.Model -> Bool
applyFilter All _ = True
applyFilter Completed (MkModel _ True _) = True
applyFilter Active (MkModel _ False _) = True
applyFilter _  _ = False


encodeTodos : List (String, Item.Model) -> Json
encodeTodos xs =
  xs
    |> map encodePair
    |> Encode.array
  where
    encodePair : (String, Item.Model) -> Json
    encodePair (id, (MkModel title completed _)) = 
      object
      [ ("id", string id)
      , ("title", string title)
      , ("completed", bool completed)
      ]

todosDecoder : Decoder $ List (String, Item.Model)
todosDecoder = Decode.array $ do
  id <- at ["id"] string
  title <- at ["title"] string
  completed <- at ["completed"] bool
  pure (id, Item.MkModel title completed Nothing)


fromUrl : String -> Maybe Filter
fromUrl url = case url of
  "#/" => Just All
  "#/active" => Just Active
  "#/completed" => Just Completed
  _ => Nothing


toUrl : Filter -> String
toUrl filter = case filter of
  All => "#/"
  Active => "#/active"
  Completed => "#/completed"


export
record Model where
  constructor MkModel
  title : String
  todos : List (String, Item.Model)
  filter : Filter
  
export
data Action
  = Edit String
  | SetFilter Filter
  | ToggleAll Bool
  | ClearCompleted
  | Todo String Item.Action
  | KeyPress Int
  | HashChange String
  | BeforeUnload


export
init : List (String, Item.Model) -> Filter -> Main.Model
init =
  MkModel ""

export
update : Main.Action -> Update Main.Model Main.Action ()
update action = case action of
  (Edit x) => modifyModel {model=Main.Model} $ record { title = x }
  (SetFilter x) => modifyModel $ record { filter = x }
  (ToggleAll check) => modifyModel $ record { todos $= map (\(id, model) => (id, record { completed = check } model)) }
  (ClearCompleted) => modifyModel $ record { todos $= filter (not . completed . snd) }
  (KeyPress 13) => do
    model <- getModel
    case trim (Main.Model.title model) of
      "" => pure ()
      trimmed => do
        id <- lift $ genUUID4
        modifyModel $ record { todos $= flip Prelude.List.(++) [(id, Item.init trimmed)] }
        modifyModel {model=Main.Model} $ record { title = "" }
  (KeyPress _) => pure ()
  (HashChange hash) =>
    case fromUrl hash of
      (Just x) => modifyModel $ record { filter = x }
      Nothing => do
        lift $ fixHash (toUrl All)
        modifyModel $ record { filter = All }
  BeforeUnload => do
    model <- getModel
    lift $ localStorageSetItem "todomvc-idris-elm" $ encodeTodos (todos model)
    pure ()
  (Todo id Destroy) => modifyModel $ record { todos $= filter (\(x, _) => not (x == id)) }
  (Todo id action') => do
    model <- getModel
    (Just (_, model')) <- pure $ find ((==) id . fst) (todos model) | Nothing => pure ()
    (nextModel, commands) <- lift $ runUpdate (Item.update action') model'
    batchCommand $ map (map $ Todo id) commands
    modifyModel $ record { todos $= map (\p => if id == fst p then (id, nextModel) else p) }

export
view : Main.Model -> Html Main.Action
view (MkModel title todos filter) =
  div
  [ subscribe "beforeunload" $ addEventListener window "beforeunload" (pure BeforeUnload)
  , subscribe "hashChange" $ hashChange HashChange
  ]
  [ section [ class' "todoapp" ]
    [ viewHeader
    , viewMain
    , viewFooter
    ]
  , footerInfo
  ]
  
  where
  itemsLeft : Nat
  itemsLeft =
    foldl (\acc, (_, model) => if not (completed model) then acc + 1 else acc) 0 todos
    
  viewHeader : Html Main.Action
  viewHeader =
    header [ class' "header" ]
    [ h1_ [ text "todos" ]
    , input
      [ class' "new-todo"
      , placeholder "What needs to be done?"
      , autofocus True
      , value title
      , onInput Edit
      , on "keypress" (map KeyPress keyCode)
      ] []
    ]
  
  viewMain : Html Main.Action
  viewMain =
    section
    [ class' "main"
    , classList [ ("hidden", isNil todos) ]
    ]
    [ input [ type "checkbox", id "toggle-all", class' "toggle-all", onCheck ToggleAll ] []
    , label [ for "toggle-all" ] [ text "Mark all as completed" ]
    , ul [ class' "todo-list" ]
      $ todos
        |> Prelude.List.filter (applyFilter filter . snd)
        |> map (\(id, todo) => Item.view todo |> map (Todo id))
        |> cast {to=ElmList (Html Action)}
    ]

  viewFilter : Filter -> Html Main.Action
  viewFilter x =
    li_
    [ a [ classList [("selected", x == filter)], href $ toUrl x ] [ text $ show x ] ]
    
  viewFooter : Html Main.Action
  viewFooter =
    footer
    [ class' "footer"
    , classList [ ("hidden", isNil todos) ]
    ]
    [ span [ class' "todo-count" ] [ strong_ [ text $ show $ itemsLeft ], text $ pluralize itemsLeft " item left" " items left" ]
    , ul [ class' "filters" ]
      $ [ All, Active, Completed ] |> map viewFilter
    , button [ class' "clear-completed", onClick ClearCompleted ] [ text "Clear completed" ]
    ]
    
  footerInfo : Html msg
  footerInfo =
    footer
    [ class' "info" ]
    [ p_ [ text "Double-click to edit a todo" ]
    , p_ [ text "Created by ", a [ href "https://github.com/lagunoff" ] [ text "Vlad Lagunov" ] ]
    , p_ [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
    ]

main : JS_IO ()
main = do
  todos <- localStorageGetItem "todomvc-idris-elm" todosDecoder
  initialize
  hash <- readHash
  let filter = maybe All id (fromUrl hash)
  let program = MkProgram (Main.init (either (const []) id todos) filter) update view
  fullscreen program
