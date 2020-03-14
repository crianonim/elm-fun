module Main exposing (..)

import Browser
import Debug exposing (..)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }





-- MODEL


type alias Model =
    { turn : Int
    , name : String
    , rooms : List Room
    , currentRoom : String
    , btnName : String
    }


type alias Room =
    { id : String
    , name : String
    , exits : List Exit
    , visited : Bool
    }


type alias Exit =
    { name : String
    , exitId : String
    }


init : Model
init =
    { turn = 0
    , name = "Jan"
    , rooms =
        [ Room "cell" "Cell" [ Exit "North" "corridor" ] False
        , Room "corridor" "Corridor" [ Exit "North" "safety", Exit "South" "cell" ] False
        , Room "safety" "Safety" [ Exit "South" "corridor" ] False
        ]
    , currentRoom = "cell"
    , btnName = "No"
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | NameChange String
    | ButtonPressed String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | turn = model.turn + 1 }

        Decrement ->
            { model | turn = model.turn - 1 }

        NameChange newName ->
            { model
                | name = newName
            }

        ButtonPressed buttonName ->
            let
                room =
                    findRoom model.currentRoom model.rooms

                exit =
                    case room of
                        Just r ->
                            findInList (\e -> e.name == buttonName) r.exits

                        Nothing ->
                            Nothing
            in
            case exit of
                Just e ->
                    { model
                        | currentRoom = e.exitId
                        , rooms =
                            List.map
                                (\r ->
                                    if log "ID" r.id == model.currentRoom then
                                        { r | visited = True }

                                    else
                                        r
                                )
                                model.rooms
                    }

                Nothing ->
                    log "BAD" model



-- VIEW


view : Model -> Html Msg
view model =
    let
        room =
            findInList (\r -> r.id == model.currentRoom) model.rooms
    in
    case room of
        Just r ->
            div []
                [ div [] [ text model.btnName ]
                , button [ onClick Decrement ] [ text "-" ]
                , div [] [ text (String.fromInt model.turn) ]
                , button [ onClick Increment ] [ text "+" ]
                , input [ value model.name, onInput NameChange ] []
                , div [] [ text model.name ]
                , viewRoomItem r
                ]

        Nothing ->
            div [] [ text "Bad" ]


viewOption : String -> Html msg
viewOption option =
    div [] [ text option ]


viewRoomItem : Room -> Html Msg
viewRoomItem room =
    div []
        [ text
            ("You are in "
                ++ room.name
                ++ (if room.visited then
                        "and you already visited it before."

                    else
                        ""
                   )
            )
        , p [] [ text "The exits are:" ]
        , div [] (List.map viewRoomExit room.exits)
        ]


viewRoomExit : Exit -> Html Msg
viewRoomExit e =
    button [ onClick (ButtonPressed e.name) ] [ text e.name ]


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
    List.head (List.filter predicate list)


findRoom : String -> List Room -> Maybe Room
findRoom roomName list =
    findInList (\r -> r.id == roomName) list
