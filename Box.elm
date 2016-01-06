module Box where

import Html exposing (..)
import Html.Attributes exposing (attribute, style, value, type')
import Html.Events exposing (on, targetValue)
import String exposing (toInt)

type alias Color = String
type alias Width = Int


-- MODEL

type alias Model =
  { color : Color
  , width : Width
  }

init : Color -> Width -> Model
init color width =
  { color = color
  , width = width
  }


-- UPDATE

type Action
  = ChangeColor Color
  | ChangeWidth Width

update : Action -> Model -> Model
update action model =
  case action of
    ChangeColor newColor ->
      { model |
        color = newColor
      }

    ChangeWidth newWidth ->
      { model |
        width = newWidth
      }


-- VIEW

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address action =
  on "input" targetValue (\val -> Signal.message address (action val))
  --alternatively using the composition operator:
  --on "input" targetValue (Signal.message address << action)

onRangeChange : Signal.Address a -> (Int -> a) -> Attribute
onRangeChange address action =
  let
    -- Convert to an int and use 0 if an error occurs
    -- https://www.reddit.com/r/elm/comments/3c012n/noob_question_about_toint/cssc6kq
    safeToInt = toInt >> Result.toMaybe >> Maybe.withDefault 0
  in
    on "input" targetValue (\val -> Signal.message address (action (safeToInt val)))
    --or with the composition operator:
    --on "input" targetValue (Signal.message address << action << safeToInt)

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ boxStyle model.color model.width ]
    [ div
      []
      [ input
        [ type' "text"
        , value model.color
        , onInput address ChangeColor
        ]
        []
      ]
    , div
      []
      [ input
        [ type' "range"
        , attribute "value" (toString model.width)
        , Html.Attributes.min "200"
        , Html.Attributes.max "1000"
        , onRangeChange address ChangeWidth
        ]
        []
      ]
    , div
      []
      [ text (toString model.width) ]
    ]


boxStyle : Color -> Width -> Attribute
boxStyle backgroundColor width =
  style
    [ ("background-color", backgroundColor)
    , ("width", (toString width) ++ "px")
    , ("border", "1px solid #000")
    , ("height", "70px")
    , ("display", "block")
    , ("padding", "20px")
    , ("margin", "10px 0")
    ]
