module Hello exposing (main)

import Array exposing (Array)
import Bytes
import Eco.Console
import Eco.File
import Eco.IO.Error exposing (IOError(..))
import Eco.Process
import Image exposing (Pixel)
import Image.Advanced
import Platform
import Random
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
    ( Eco.Console.log "Modifying image..." {}
    , Eco.File.getCwd
        |> Task.mapError (\_ -> FileNotFound "")
        |> Task.andThen
            (\path ->
                Eco.File.readBytes (path ++ "/outline-guy-sit-frame-1.png")
                    |> Task.andThen
                        (\bytes ->
                            case Image.decode bytes of
                                Just image ->
                                    List.range 2 5
                                        |> List.map
                                            (\index ->
                                                Image.toArray2d image
                                                    |> boilEffect index
                                                    |> Image.fromArray2d
                                                    |> Image.toPng
                                                    |> Eco.File.writeBytes
                                                        (path
                                                            ++ "/outline-guy-sit-frame-"
                                                            ++ String.fromInt index
                                                            ++ ".png"
                                                        )
                                            )
                                        |> Task.sequence
                                        |> Task.map (\_ -> ())

                                Nothing ->
                                    Eco.Console.write Eco.Console.stdout "Couldn't parse image" |> Task.mapError (\_ -> FileNotFound "")
                        )
            )
        |> Task.andThen (\_ -> Eco.Process.exit Eco.Process.ExitSuccess |> Task.mapError (\_ -> FileNotFound ""))
        |> Task.onError
            (\error ->
                case error of
                    FileNotFound text ->
                        Eco.Console.write Eco.Console.stdout ("FileNotFound " ++ text)

                    PermissionDenied text ->
                        Eco.Console.write Eco.Console.stdout ("PermissionDenied " ++ text)

                    NotADirectory text ->
                        Eco.Console.write Eco.Console.stdout ("NotADirectory " ++ text)

                    IsADirectory text ->
                        Eco.Console.write Eco.Console.stdout ("IsADirectory " ++ text)

                    AlreadyExists text ->
                        Eco.Console.write Eco.Console.stdout ("AlreadyExists " ++ text)

                    NoSpaceLeft maybeString ->
                        Eco.Console.write Eco.Console.stdout ("NoSpaceLeft " ++ Maybe.withDefault "" maybeString)

                    TooManyOpenFiles ->
                        Eco.Console.write Eco.Console.stdout "TooManyOpenFiles"

                    BrokenPipe maybeString ->
                        Eco.Console.write Eco.Console.stdout ("BrokenPipe " ++ Maybe.withDefault "" maybeString)

                    BadFileDescriptor ->
                        Eco.Console.write Eco.Console.stdout "BadFileDescriptor"

                    OtherIOError record ->
                        Eco.Console.write Eco.Console.stdout ("OtherIOError " ++ record.message)
            )
        |> Task.attempt (\_ -> Exited)
    )


boilEffect : Int -> Array (Array Pixel) -> Array (Array Pixel)
boilEffect index array =
    Random.step
        (Random.map4
            (\xScale yScale xOffset yOffset ->
                Array.indexedMap
                    (\y row ->
                        Array.indexedMap
                            (\x pixel ->
                                get
                                    (round (sin (toFloat (xOffset + x) / toFloat xScale) * 1.2) + x)
                                    (round (cos (toFloat (yOffset + y) / toFloat yScale) * 1.2) + y)
                                    array
                            )
                            row
                    )
                    array
            )
            (Random.int 6 8)
            (Random.int 6 8)
            (Random.int 0 10)
            (Random.int 0 10)
        )
        (Random.initialSeed (123 + index))
        |> Tuple.first


get : Int -> Int -> Array (Array Pixel) -> Pixel
get x y array =
    case Array.get y array of
        Just row ->
            case Array.get x row of
                Just pixel ->
                    pixel

                Nothing ->
                    0

        Nothing ->
            0


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
