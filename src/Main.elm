module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, input, li, p, text, ul)
import Html.Attributes exposing (checked, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

todosUrl : String
todosUrl =
    "http://localhost:9228/todos"



-- Helper function to convert HTTP errors to user-friendly strings


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus response ->
            "Bad response: " ++ String.fromInt response

        Http.BadBody message ->
            "Bad body: " ++ message



-- MODEL


type alias ToDo =
    { id : String
    , title : String
    , completed : Bool
    }


type alias Model =
    { todos : List ToDo
    , input : String
    , editingId : Maybe String
    , editingTitle : Maybe String
    , error : Maybe String
    }


init : Model
init =
    { todos = []
    , input = ""
    , editingId = Nothing
    , editingTitle = Nothing
    , error = Nothing
    }



-- UPDATE


type Msg
    = GotTodos (Result Http.Error (List ToDo))
    | CreateToDo
    | NewInput String
    | EditToDo String
    | NewTitle String
    | SaveEdit String
    | CancelEdit
    | DeleteToDo String
    | CompleteToDo String Bool
    | FetchTodos
    | HttpError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodos result ->
            case result of
                Ok todos ->
                    ( { model | todos = todos, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just (errorToString error) }, Cmd.none )

        CreateToDo ->
            ( { model | input = "" }, createToDo model.input )

        NewInput input ->
            ( { model | input = input }, Cmd.none )

        EditToDo id ->
            let
                maybeTodo =
                    List.head (List.filter (\todo -> todo.id == id) model.todos)
            in
            case maybeTodo of
                Just todo ->
                    ( { model | editingId = Just id, editingTitle = Just todo.title }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewTitle title ->
            ( { model | editingTitle = Just title }, Cmd.none )

        SaveEdit id ->
            case model.editingTitle of
                Just title ->
                    ( { model | editingId = Nothing, editingTitle = Nothing }, Cmd.batch [ editToDo id title, fetchTodos ] )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editingId = Nothing, editingTitle = Nothing }, Cmd.none )

        DeleteToDo id ->
            ( { model | editingId = Nothing, editingTitle = Nothing }, Cmd.batch [ deleteToDo id, fetchTodos ] )

        CompleteToDo id completed ->
            ( model, completeToDo id completed )

        FetchTodos ->
            ( model, fetchTodos )

        HttpError error ->
            ( { model | error = Just (errorToString error) }, Cmd.none )



-- VIEW

sortTodosByTitle : List ToDo -> List ToDo
sortTodosByTitle todos =
    List.sortBy (\todo -> todo.title) todos

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "My To Do App" ]
        , viewAddToDo model
        , ul [] (List.map (viewToDo model) (sortTodosByTitle model.todos))
                ]


viewAddToDo : Model -> Html Msg
viewAddToDo model =
    div []
        [ case model.error of
            Nothing ->
                text ""

            Just error ->
                div [] [ text ("Error: " ++ error) ]
        , form [ onSubmit CreateToDo ]
            [ input [ type_ "text", placeholder "What needs to be done?", onInput NewInput, value model.input ] []
            , button [ type_ "submit", disabled (String.isEmpty model.input) ] [ text "Add" ]
            ]
        ]


viewToDoListItem : ToDo -> Html Msg
viewToDoListItem todo =
    li []
        [ input [ type_ "checkbox", checked todo.completed, onClick (CompleteToDo todo.id (not todo.completed)) ] []
        , p [] [ text todo.title ]
        , button [ onClick (EditToDo todo.id) ] [ text "Edit" ]
        , button [ onClick (DeleteToDo todo.id) ] [ text "Delete" ]
        ]


viewEditToDo : ToDo -> String -> Html Msg
viewEditToDo todo title =
    li []
        [ form [ onSubmit (SaveEdit todo.id) ]
            [ input [ type_ "checkbox", checked todo.completed, onClick (CompleteToDo todo.id (not todo.completed)) ] []
            , input [ type_ "text", onInput NewTitle, value title ] []
            , button [ type_ "submit" ] [ text "Save" ]
            , button [ onClick CancelEdit ] [ text "Cancel" ]
            ]
        ]


viewToDo : Model -> ToDo -> Html Msg
viewToDo model todo =
    case model.editingId of
        Just id ->
            if id == todo.id then
                case model.editingTitle of
                    Just title ->
                        viewEditToDo todo title

                    Nothing ->
                        text ""

            else
                viewToDoListItem todo

        Nothing ->
            viewToDoListItem todo



-- HTTP


todoDecoder : Decode.Decoder ToDo
todoDecoder =
    Decode.map3 ToDo
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "completed" Decode.bool)


todoEncoder : ToDo -> Encode.Value
todoEncoder todo =
    Encode.object
        [ ( "id", Encode.string todo.id )
        , ( "title", Encode.string todo.title )
        , ( "completed", Encode.bool todo.completed )
        ]


fetchTodos : Cmd Msg
fetchTodos =
    Http.get
        { url = todosUrl
        , expect = Http.expectJson GotTodos (Decode.list todoDecoder)
        }


createToDo : String -> Cmd Msg
createToDo title =
    Http.request
        { method = "POST"
        , headers = []
        , url = todosUrl
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "title", Encode.string title )
                    , ( "completed", Encode.bool False )
                    ]
        , expect = Http.expectWhatever (always FetchTodos)
        , timeout = Nothing
        , tracker = Nothing
        }


editToDo : String -> String -> Cmd Msg
editToDo id title =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "http://localhost:9228/todos/" ++ id
        , body = Http.jsonBody (Encode.object [ ( "title", Encode.string title ) ])
        , expect = Http.expectWhatever (always FetchTodos)
        , timeout = Nothing
        , tracker = Nothing
        }


completeToDo : String -> Bool -> Cmd Msg
completeToDo id completed =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "http://localhost:9228/todos/" ++ id
        , body = Http.jsonBody (Encode.object [ ( "completed", Encode.bool completed ) ])
        , expect = Http.expectWhatever (always FetchTodos)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteToDo : String -> Cmd Msg
deleteToDo id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:9228/todos/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (always FetchTodos)
        , timeout = Nothing
        , tracker = Nothing
        }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, fetchTodos )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
