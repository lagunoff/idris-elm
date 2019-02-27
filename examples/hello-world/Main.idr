module Main

import Elm.Html
import Elm.Attributes
import Elm.Events
import Elm.Decode
import Elm.Platform
import Utils
import Data.IORef

%default total

    
colors : List String
colors =
  ["rgb(173, 192, 84)","rgb(22, 153, 190)","rgb(22, 93, 24)","rgb(199, 232, 42)","rgb(235, 206, 57)","rgb(225, 57, 149)","rgb(255, 134, 157)","rgb(231, 251, 35)","rgb(148, 122, 45)","rgb(227, 10, 30)","rgb(97, 22, 125)","rgb(239, 243, 10)","rgb(155, 247, 3)","rgb(199, 31, 74)","rgb(109, 198, 34)","rgb(170, 52, 228)","rgb(61, 44, 247)","rgb(118, 45, 39)","rgb(248, 116, 17)","rgb(27, 184, 238)","rgb(117, 23, 222)"]


record Model where
  constructor MkModel
  colorNo : (n : Nat ** InBounds n Main.colors)
    
data Msg
  = NextColor
  
init : Model  
init =
  MkModel (0 ** InFirst)
  
  
update : Msg -> Model -> Model
update NextColor = record { colorNo $= listCycle colors }


view : Model -> Html Msg
view (MkModel (n ** _)) =
  div [ class' "root" ]
  [ h2
    [ style [("color" := index n colors)]
    , on "mouseenter" (pure NextColor)
    ]
    [ text "Hello World"
    ]
  , node_ "style" [ text css ]
  ]
  
  where
  css : String
  css =
    """
    html, body { margin: 0; height: 100%; }
    .root { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; }
    .root > h2 { font-size: 48px; margin: 0; font-family: "Helvetica", Arial, sans-serif; font-weight: 600; border: dashed 4px rgba(0,0,0,0.12); cursor: default; padding: 8px 16px; }
    """


eval : IORef Model -> Program Msg -> Msg -> JS_IO ()
eval modelRef inst msg = do
  model <- readIORef' modelRef
  let nextModel = update msg model
  writeIORef' modelRef nextModel
  actuate inst (view nextModel)


main : JS_IO ()
main = do
  let model = init
  fullscreen' model view eval
  pure ()


