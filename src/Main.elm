module Main exposing (..)

import Browser
import Debug exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Room =
    { id : String
    , name : String
    , exits : List Exit
    , visited : Bool
    , entities : List String
    }


type alias Exit =
    { name : String
    , exitId : String
    }


type alias Stats =
    { hpMax : Int
    , hp : Int
    , attack : Int
    }


type alias Id =
    String


type Entity
    = Mob Id Stats String


type Page
    = StartPage
    | PlayingPage
    | LostPage


type alias Model =
    { turn : Int
    , name : String
    , rooms : List Room
    , currentRoom : String
    , page : Page
    , entities : Dict String Entity
    }


init : Model
init =
    { turn = 0
    , name = "Jan"
    , rooms =
        [ Room "cell" "Cell" [ Exit "North" "corridor" ] False []
        , Room "corridor" "Corridor" [ Exit "North" "safety", Exit "South" "cell" ] False [ "#g1" ]
        , Room "safety" "Safety" [ Exit "South" "corridor" ] False []
        ]
    , currentRoom = "cell"
    , page = StartPage
    , entities = Dict.fromList [ ( "player", Mob "player" (Stats 20 20 4) "Player" ), ( "#g1", Mob "#g1" (Stats 10 10 3) "Goblin" ) ]
    }



-- UPDATE


type Msg
    = NameChange String
    | ButtonPressed String
    | Rest
    | StartGame
    | Attack Id


update : Msg -> Model -> Model
update msg model =
    case msg of
        Attack id ->
            attackMsg model "player" id

        Rest ->
            playTurn 1 model

        StartGame ->
            { model | page = PlayingPage }

        NameChange newName ->
            { model
                | name = newName
            }

        ButtonPressed buttonName ->
            let
                room =
                    findRoom model.currentRoom model.rooms

                exit =
                    Maybe.andThen (\r -> findInList (\e -> e.name == buttonName) r.exits) room

                -- case room of
                --     Just r ->
                --         findInList (\e -> e.name == buttonName) r.exits
                --     Nothing ->
                --         Nothing
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
                        |> playTurn 1

                Nothing ->
                    log "BAD" model



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        StartPage ->
            div []
                [ p [] [ text "What is your name?" ]
                , input [ value model.name, onInput NameChange ] []
                , button [ onClick StartGame ] [ text "Start" ]
                ]

        LostPage ->
            div [] [ text "You lost." ]

        PlayingPage ->
            let
                room =
                    findInList (\r -> r.id == model.currentRoom) model.rooms
            in
            case room of
                Just r ->
                    div []
                        [ div [] [ text (String.fromInt model.turn) ]
                        , div [] [ text model.name ]
                        , viewRoomItem model r
                        ]

                Nothing ->
                    div [] [ text "Bad" ]


viewOption : String -> Html msg
viewOption option =
    div [] [ text option ]


viewRoomItem : Model -> Room -> Html Msg
viewRoomItem model room =
    div []
        [ text
            ("You are in "
                ++ room.name
                ++ (if room.visited then
                        " and you have already been here."

                    else
                        ""
                   )
            )
        , if List.isEmpty room.entities then
            p [] [ text "Nothing is here" ]

          else
            div [] (List.map viewEntity (List.map (findEntity model) room.entities))
        , p [] [ text "The exits are:" ]
        , div [] (List.map viewRoomExit room.exits)
        , button [ onClick Rest ] [ text "Rest" ]
        ]


viewEntity : Maybe Entity -> Html Msg
viewEntity entity =
    case entity of
        Just (Mob id stats name) ->
            div [] [ text name, viewStats stats, button [ onClick (Attack id) ] [ text "Attack!" ] ]

        Nothing ->
            p [] [ text "Bad entity" ]


viewStats : Stats -> Html msg
viewStats stats =
    let
        { hpMax, hp, attack } =
            stats
    in
    p [] [ text (String.fromInt hp ++ "/" ++ String.fromInt hpMax ++ " ATT: " ++ String.fromInt attack) ]


viewRoomExit : Exit -> Html Msg
viewRoomExit e =
    button [ onClick (ButtonPressed e.name) ] [ text e.name ]


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
    List.head (List.filter predicate list)


findRoom : String -> List Room -> Maybe Room
findRoom roomName list =
    findInList (\r -> r.id == roomName) list


playTurn : Int -> Model -> Model
playTurn turns model =
    let
        newTurn =
            model.turn + turns
    in
    { model
        | turn = newTurn
        , page =
            if newTurn > 10 then
                LostPage

            else
                model.page
    }


attackMsg : Model -> Id -> Id -> Model
attackMsg model attId defId =
    case
        Maybe.map2 (\att def -> ( att, def )) (findEntity model attId) (findEntity model defId)
    of
        Nothing ->
            model

        Just ( attacker, defender ) ->
            let
                ( att, def ) =
                    attackAction ( attacker, defender )
            in
            case def of
                Mob id _ _ ->
                    { model | entities = Dict.update id (\_ -> Just def) model.entities }


attackAction : ( Entity, Entity ) -> ( Entity, Entity )
attackAction ( attacker, defender ) =
    let
        ( _, attStats, _ ) =
            case attacker of
                Mob i s name ->
                    ( i, s, name )

        ( defId, defStats, defName ) =
            case defender of
                Mob i s name ->
                    ( i, s, name )
    in
    ( attacker, Mob defId { defStats | hp = defStats.hp - attStats.attack } defName )


findEntity : Model -> Id -> Maybe Entity
findEntity model id =
    Dict.get id model.entities
