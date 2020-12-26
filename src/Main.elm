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
    , blockedByEntity : Maybe Id
    }


type alias Stats =
    { hpMax : Int
    , hp : Int
    , attack : Int
    }


type alias Id =
    String


type alias Entity =
    { id : Id
    , stats : Stats
    , name : String
    , alive : Bool
    }


badEntity =
    Entity "bad" (Stats 0 0 0) "Bad Entity" True


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
        [ Room "cell" "Cell" [ Exit "North" "corridor" Nothing ] False []
        , Room "corridor" "Corridor" [ Exit "North" "safety" (Just "#g1"), Exit "South" "cell" Nothing ] False [ "#g1" ]
        , Room "safety" "Safety" [ Exit "South" "corridor" Nothing ] False []
        ]
    , currentRoom = "cell"
    , page = PlayingPage
    , entities = Dict.fromList [ ( "player", Entity "player" (Stats 20 20 4) "Player" True ), ( "#g1", Entity "#g1" (Stats 10 10 3) "Goblin" True ) ]
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
                |> checkHealth

        Rest ->
            playTurn 1 model
                |> heal "player"

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
               
            in
          
            case exit of
                Just e ->
                    e.blockedByEntity
                        |> Maybe.andThen (\id -> findEntity model id)
                        |> Maybe.andThen
                            (\entity ->
                                case entity.alive of
                                    True ->
                                        Just model

                                    False ->
                                        Maybe.Nothing
                            )
                        |> Maybe.withDefault (actionGo e model)

                Nothing ->
                    log "BAD" model


actionGo : Exit -> Model -> Model
actionGo exit model =
    { model
        | currentRoom = exit.exitId
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

                player =
                    Maybe.withDefault badEntity (findEntity model "player")
            in
            case room of
                Just r ->
                    div []
                        [ div [] [ text (String.fromInt model.turn) ]
                        , div [] [ text ("HP: " ++ String.fromInt player.stats.hp ++ " / " ++ String.fromInt player.stats.hpMax) ]
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
        Just { id, name, stats, alive } ->
            div []
                [ text
                    ((if alive then
                        " alive "

                      else
                        " dead "
                     )
                        ++ name
                    )
                , viewStats stats
                , if alive then
                    button [ onClick (Attack id) ] [ text "Attack!" ]

                  else
                    text ""
                ]

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
        Maybe.map2 Tuple.pair (findEntity model attId) (findEntity model defId)
    of
        Nothing ->
            model

        Just ( attacker, defender ) ->
            let
                ( att1, def1 ) =
                    attackAction ( attacker, defender )

                ( def, att ) =
                    attackAction ( def1, att1 )
            in
            { model | entities = Dict.update attId (\_ -> Just att) (Dict.update defId (\_ -> Just def) model.entities) }


attackAction : ( Entity, Entity ) -> ( Entity, Entity )
attackAction ( attacker, defender ) =
    let
        stats =
            defender.stats
    in
    ( attacker, { defender | stats = { stats | hp = Basics.max (defender.stats.hp - attacker.stats.attack) 0 } } )


findEntity : Model -> Id -> Maybe Entity
findEntity model id =
    Dict.get id model.entities


checkHealth : Model -> Model
checkHealth model =
    { model | entities = Dict.map (\k v -> { v | alive = v.stats.hp > 0 }) model.entities }


heal : Id -> Model -> Model
heal id model =
    let
        ent =
            Maybe.withDefault badEntity (findEntity model id)

        stats =
            ent.stats

        st =
            { stats | hp = Basics.min (ent.stats.hp + 1) ent.stats.hpMax }
    in
    { model | entities = Dict.insert id { ent | stats = st } model.entities }
