module RansomLetters exposing (renderLetters)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Char


renderLetters : Int -> Int -> String -> Html a
renderLetters take markTo string =
    let
        chars =
            string
                |> String.left take
                |> String.toList
                |> List.indexedMap (,)
                |> List.map (toMarkedTuple markTo)
    in
        div
            [ style [ ( "word-wrap", "break-word" ), ( "max-width", "900px" ) ] ]
            (List.map
                renderChar
                chars
            )


toMarkedTuple : Int -> ( Int, Char ) -> ( Bool, Char )
toMarkedTuple markTo ( index, char ) =
    let
        mark =
            index < markTo
    in
        ( mark, char )


renderChar : ( Bool, Char ) -> Html a
renderChar ( mark, character ) =
    let
        code =
            Char.toCode character

        color =
            let
                ( r, g, b ) =
                    if (mark) then
                        getRandomRGB code
                    else
                        ( 255, 255, 255 )
            in
                "rgb(" ++ toString r ++ "," ++ toString g ++ "," ++ toString b ++ ")"

        size =
            code - 50
    in
        span
            [ style
                [ ( "font-size", toString size ++ "px" )
                , ( "background-color", color )
                , ( "margin-right", "15px" )
                ]
            ]
            [ text (String.fromChar character) ]


getRandomRGB : Int -> ( Int, Int, Int )
getRandomRGB randomNumber =
    let
        deviation =
            Basics.round (((toFloat randomNumber) / (toFloat 90)) * 40)
    in
        ( 80, (254 - deviation) % 255, (128 + deviation) % 255 )
