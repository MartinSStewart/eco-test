module Hello exposing (main)

import Bytes
import Eco.Console
import Eco.File
import Eco.IO.Error exposing (IOError(..))
import Eco.Process
import Image
import Platform
import Task


type alias Model =
    {}


type Msg
    = Exited
    | WroteToConsole (Result IOError ())


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Eco.Console.log "Hello World!" {}
    , Eco.File.getCwd
        |> Task.mapError (\_ -> FileNotFound "")
        |> Task.andThen (\path -> Eco.File.readBytes (path ++ "/outline-guy-sit-frame-1.png"))
        |> Task.map
            (\bytes ->
                Image.decode bytes
            )
        |> Task.andThen (\_ -> Eco.Process.exit Eco.Process.ExitSuccess |> Task.mapError (\_ -> FileNotFound ""))
        |> Task.attempt (\_ -> Exited)
    )


listFilesRecursive : String -> Task.Task IOError (List String)
listFilesRecursive path =
    Eco.File.dirExists path
        |> Task.mapError (\_ -> FileNotFound "")
        |> Task.andThen
            (\isDir ->
                if isDir then
                    Eco.File.list path
                        |> Task.andThen
                            (\list ->
                                List.map (\item -> listFilesRecursive (path ++ "/" ++ item)) list
                                    |> Task.sequence
                                    |> Task.map List.concat
                            )

                else
                    Task.succeed [ path ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Exited ->
            ( model, Cmd.none )

        WroteToConsole _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
