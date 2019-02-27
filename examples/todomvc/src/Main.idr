module Main

import IdrisScript
import Elm.Html
import Elm.Attributes 
import Elm.Events
import Elm.Subs
import Control.Pipeline
import Elm.Platform
import Elm.Decode
import Elm.Encode
import Utils
import Data.IORef
import Item as Item

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
data Msg
  = Edit String
  | SetFilter Filter
  | ToggleAll Bool
  | ClearCompleted
  | Todo String Item.Msg
  | KeyPress Int
  | HashChange String
  | BeforeUnload


export
init : List (String, Item.Model) -> Filter -> Main.Model
init =
  MkModel ""

export
update : Main.Msg -> Main.Model -> JS_IO Main.Model
update msg model = case msg of
  (Edit x) =>
    pure $ record { title = x } model
     
  (SetFilter x) =>
    pure $ record { filter = x } model
     
  (ToggleAll check) =>
    pure $ record { todos $= map (\(id, model) => (id, record { completed = check } model)) } model
     
  (ClearCompleted) =>
    pure $ record { todos $= filter (not . Item.Model.completed . snd) } model
    
  (KeyPress 13) => do
    case trim (Main.Model.title model) of
      "" => pure model
      trimmed => do
        id <- genUUID4
        pure $ record { todos $= flip (++) [(id, Item.init trimmed)], title = "" } model
        
  (KeyPress _) =>
     pure model
     
  (HashChange hash) =>
    case fromUrl hash of
      (Just x) => pure $ record { filter = x } model
      Nothing => do
        fixHash (toUrl All)
        pure $ record { filter = All } model
        
  (BeforeUnload) => do
    localStorageSetItem "todomvc-idris-elm" $ encodeTodos (todos model)
    pure model
    
  (Todo id Destroy) =>
     pure $ record { todos $= filter (\(x, _) => not (x == id)) } model
     
  (Todo id msg') => do
    (Just (_, model')) <- pure $ find ((==) id . fst) (todos model) | Nothing => pure model
    (itemModel, maybeMsg) <- Item.update msg' model'
    let nextModel = record { todos $= map (\p => if id == fst p then (id, itemModel) else p) } model
    case maybeMsg of
      Just msg => update (Todo id msg) nextModel
      Nothing => pure nextModel

export
view : Main.Model -> Html Main.Msg
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
    foldl (\acc, (_, model) => if not (Item.Model.completed model) then acc + 1 else acc) 0 todos
    
  viewHeader : Html Main.Msg
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
  
  viewMain : Html Main.Msg
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
        |> cast {to=ElmList (Html Msg)}
    ]

  viewFilter : Filter -> Html Main.Msg
  viewFilter x =
    li_
    [ a [ classList [("selected", x == filter)], href $ toUrl x ] [ text $ show x ] ]
    
  viewFooter : Html Main.Msg
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

eval : IORef Main.Model -> Program Main.Msg -> Main.Msg -> JS_IO ()
eval modelRef inst msg = do
  model <- readIORef' modelRef
  nextModel <- update msg model
  writeIORef' modelRef nextModel
  actuate inst (view nextModel)


main : JS_IO ()
main = do
  todos <- localStorageGetItem "todomvc-idris-elm" todosDecoder
  hash <- readHash
  let filter = maybe All id (fromUrl hash)
  let model = Main.init (either (const []) id todos) filter
  fullscreen' model view eval
  pure ()
