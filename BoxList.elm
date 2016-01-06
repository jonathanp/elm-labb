module BoxList where

import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Color
import Color

import Box


-- MODEL

type alias Model =
  { boxes : List ( ID, Box.Model )
  , nextID: ID
  }


type alias ID = Int


init : Model
init =
  { boxes = []
  , nextID = 0
  }


newBox : ID -> ( ID, Box.Model )
newBox id =
  let
    seed = Random.initialSeed id

    rgbToColor : { alpha : Float, blue : Int, green : Int, red : Int } -> Box.Color
    rgbToColor rgb =
      "rgb(" ++
        (toString rgb.red) ++ "," ++
        (toString rgb.green) ++ "," ++
        (toString rgb.blue) ++
      ")"

    randomColor : Box.Color
    randomColor =
      Random.generate Random.Color.rgb seed
        |> fst
        |> Color.toRgb
        |> rgbToColor

    randomWidth =
      Random.generate (Random.int 200 1000) seed
        |> fst
  in
    (id, Box.init randomColor randomWidth)


-- UPDATE

type Action
  = Add
  | AddMultiple Int
  | RemoveAll
  | Modify ID Box.Action


update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      { model |
        boxes = model.boxes ++ [ newBox model.nextID ],
        nextID = model.nextID + 1
      }

    AddMultiple amount ->
      let
        newBoxIds = [model.nextID .. model.nextID + amount]
        newBoxes = List.map (\id -> newBox id) newBoxIds
      in
        { model |
          boxes = model.boxes ++ newBoxes,
          nextID = model.nextID + amount + 1
        }

    RemoveAll ->
      init

    Modify id boxAction ->
      let
        updateBox (boxId, boxModel) =
          if (boxId == id)
            then (boxId, Box.update boxAction boxModel)
            else (boxId, boxModel)
      in
        { model |
          boxes = List.map updateBox model.boxes
        }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    boxes = List.map (renderBox address) model.boxes
    addButton = button [ onClick address Add ] [ text "Add new item"]
    addManyButton = button [ onClick address (AddMultiple 10) ] [ text "Add many new items"]
    removeAllButton = button [ onClick address RemoveAll ] [ text "Remove all items"]
  in
    div
      []
      [ div
        []
        [ addButton, addManyButton, removeAllButton ]
      , div
        []
        boxes
      ]


renderBox : Signal.Address Action -> (ID, Box.Model) -> Html
renderBox address (id, model) =
  Box.view (Signal.forwardTo address (Modify id)) model
