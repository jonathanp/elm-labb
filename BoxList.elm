module BoxList where

import Html exposing (..)
import Html.Events exposing (onClick)

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


-- UPDATE

type Action
  = Add
  | RemoveAll
  | Modify ID Box.Action


update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      let
        randomColor = "red"
        randomWidth = 400
        newBox =
          (model.nextID, Box.init randomColor randomWidth)
      in
        { model |
          boxes = model.boxes ++ [ newBox ],
          nextID = model.nextID + 1
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
    removeAllButton = button [ onClick address RemoveAll ] [ text "Remove all items"]
  in
    div
      []
      [ div
        []
        [ addButton , removeAllButton ]
      , div
        []
        boxes
      ]

renderBox : Signal.Address Action -> (ID, Box.Model) -> Html
renderBox address (id, model) =
  Box.view (Signal.forwardTo address (Modify id)) model
