module Main exposing (..)

import Html exposing (Html, div, h1, text, label, input, i)
import Html.Attributes as Attributes exposing (class, type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import String


---- MODEL ----

type alias Model = {
        friendsList : List String,
        inputValue : String
    }

init : (Model, Cmd msg)
init =
    ( { friendsList = ["Bruce Bannner", "Tony Stark"], inputValue = "" }, Cmd.none )


---- UPDATE ----
type Msg
    = RemoveFriend String
    | UpdateInputValue String
    | AddFriend

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        RemoveFriend name ->
            let
                fl =
                    model.friendsList
                    |> List.filter ((/=) name)
            in
                ({ model | friendsList = fl }, Cmd.none)

        UpdateInputValue inp ->
            ({ model | inputValue = inp }, Cmd.none)

        AddFriend ->
            if String.isEmpty model.inputValue then
                (model, Cmd.none)
            else
                let
                    n = model.inputValue
                in
                    ({ model | friendsList = n :: model.friendsList, inputValue = "" }, Cmd.none)

---- VIEW ----

cardView : String -> Html Msg
cardView name =
    div [ class "card" ] [
        div [ class "content" ] [
            i [ class "right floated remove red icon link"
                , onClick <| RemoveFriend name ] []
            , div [ class "description" ] [ text name ]
        ]
    ]

view {inputValue, friendsList } =
    div [ class "ui main text container" ] [
            h1 [ class "ui header" ] [ text "Friends List" ]
            , div [ class "ui segment"] [
                div [ class "ui form" ] [
                    div [ class "field" ] [
                        label [] [ text "Name" ]
                        , input [ type_ "text"
                                  , placeholder "Name"
                                  , onInput UpdateInputValue
                                  , value inputValue ] []
                    ]
                    , div [ class "ui button", onClick AddFriend ] [ text "Add a friend" ]
                ]
            ]
            , div [ class "ui two column cards" ] 
                 (List.map cardView friendsList)
        ]



---- PROGRAM ----

main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
