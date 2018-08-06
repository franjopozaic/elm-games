module Typist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import Random
import Char
import String
import Time exposing (Time, millisecond, second)
import Keyboard
import Debug


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias RGB =
    { r : Int
    , g : Int
    , b : Int
    }


type alias Character =
    { value : String
    , color : RGB
    , size : Int
    , matched : Bool
    }


type alias Model =
    { characters : List Character
    , keys : List String
    , stage : GameStage
    , time : Float
    }


type GameStage
    = Setup
    | Ready
    | Play
    | GameOver


type Msg
    = AddRandomLetter Int
    | Tick Time
    | KeyMsg Keyboard.KeyCode
    | Start
    | Count Time
    | Reset


init : ( Model, Cmd Msg )
init =
    ( Model [] [] Setup 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | stage = Play }, Cmd.none )

        Reset ->
            ( { model | characters = [], time = 0, stage = Setup }, Cmd.none )

        Count t ->
            if (List.all (\c -> c.matched) model.characters) then
                ( { model | stage = GameOver }, Cmd.none )
            else
                ( { model | time = model.time + 0.01 }, Cmd.none )

        AddRandomLetter randomNumber ->
            let
                color =
                    getRandomRGB randomNumber

                newChar =
                    Character (getRandomChar randomNumber) color (randomNumber + 48) False
            in
                ( { model | characters = List.append model.characters [ newChar ] }, Cmd.none )

        Tick time ->
            if (List.length model.characters < 10) then
                ( model, Random.generate AddRandomLetter (Random.int 0 maxRand) )
            else
                ( { model | stage = Ready }, Cmd.none )

        KeyMsg code ->
            ( { model
                | characters =
                    List.map
                        (\c ->
                            if isMatch c code then
                                markAsMatched c
                            else
                                c
                        )
                        model.characters
                , keys = toCharString code :: model.keys
                , stage = Play
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.stage of
        Setup ->
            Time.every (millisecond * 30) Tick

        Play ->
            Sub.batch [ Keyboard.downs KeyMsg, Time.every (millisecond * 10) Count ]

        Ready ->
            Keyboard.downs KeyMsg

        GameOver ->
            Sub.none


view : Model -> Html Msg
view model =
    div [ style [ ( "margin-left", "100px" ), ( "margin-top", "80px" ) ] ]
        [ (div []
            [ (text ("Time: " ++ (String.left 5 (toString model.time)))) ]
          )
        , (div
            [ style [ ( "word-wrap", "break-word" ), ( "max-width", "700px" ) ] ]
            (List.map toLetter model.characters)
          )
        , (div [] [ button [ onClick Reset ] [ text "Reset" ] ])
        ]


toLetter : Character -> Html Msg
toLetter char =
    let
        color =
            "rgb(" ++ toString char.color.r ++ "," ++ toString char.color.g ++ "," ++ toString char.color.b ++ ")"
    in
        span [ style [ ( "font-size", toString char.size ++ "px" ), ( "background-color", color ), ( "margin-right", "15px" ) ] ] [ text char.value ]


toCharString : Int -> String
toCharString code =
    String.fromChar (Char.fromCode code)


isMatch : Character -> Int -> Bool
isMatch char code =
    String.toUpper char.value == toCharString code


markAsMatched : Character -> Character
markAsMatched char =
    { char | color = RGB 136 136 136, matched = True }


getRandomRGB : Int -> RGB
getRandomRGB randomNumber =
    let
        scale =
            Basics.round ((toFloat randomNumber / toFloat maxRand) * 60)
    in
        RGB (80) (255 - scale) (128 + scale)


maxRand : Int
maxRand =
    35


getRandomChar : Int -> String
getRandomChar randomNumber =
    if (randomNumber < 10) then
        toString randomNumber
    else
        toCharString (randomNumber + 55)
