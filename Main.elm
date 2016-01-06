import Box exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = init "red" 400
    , update = update
    , view = view
    }
