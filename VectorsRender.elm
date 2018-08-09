module VectorsRender exposing (..)

import VectorsModels exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


renderSettings : Settings -> Html Msg
renderSettings settings =
    div []
        [ div []
            [ button [ onClick AddObjectManual ] [ (Html.text "Generate a circle") ] ]
        , Html.form []
            [ fieldset []
                [ div []
                    [ Html.text "Is bounded" ]
                , p []
                    [ input [ Html.Attributes.type_ "checkbox", defaultValue (toString settings.isBounded), onInput (SettingsChange IsBounded) ]
                        []
                    ]
                , div []
                    [ Html.text "Number of objects" ]
                , p []
                    [ label []
                        [ Html.text "From " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.maxNumberOfObjects), onInput (SettingsChange MaxNumberOfObjects) ]
                        []
                    ]
                , div []
                    [ Html.text "Size" ]
                , p []
                    [ label []
                        [ Html.text "From " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.sizeRange.min), onInput (SettingsChange MinSize) ]
                        []
                    ]
                , p []
                    [ label []
                        [ Html.text "To " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.sizeRange.max), onInput (SettingsChange MaxSize) ]
                        []
                    ]
                , div []
                    [ Html.text "Velocity" ]
                , p []
                    [ label []
                        [ Html.text "From " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.velocityRange.min), onInput (SettingsChange MinVelocity) ]
                        []
                    ]
                , p []
                    [ label []
                        [ Html.text "To " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.velocityRange.max), onInput (SettingsChange MaxVelocity) ]
                        []
                    ]
                , div []
                    [ Html.text "Gradient" ]
                , p []
                    [ label []
                        [ Html.text "From " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.gradientRange.min), onInput (SettingsChange MinGradient) ]
                        []
                    ]
                , p []
                    [ label []
                        [ Html.text "To " ]
                    , input [ Html.Attributes.type_ "number", defaultValue (toString settings.gradientRange.max), onInput (SettingsChange MaxGradient) ]
                        []
                    ]
                ]
            ]
        ]


renderSvg : List { a | object : Object } -> Html Msg
renderSvg objects =
    svg
        [ Svg.Attributes.width (toString boxWidth)
        , Svg.Attributes.height (toString boxHeight)
        , fill "white"
        , stroke "black"
        , strokeWidth "3"
        , Html.Attributes.style
            [ ( "padding-left", "50px" ), ( "padding-top", "50px" ) ]
        ]
        (objects
            |> List.map (\c -> c.object)
            |> List.map renderObject
        )


renderObject : Object -> Svg Msg
renderObject o =
    case o of
        Circle info ->
            circle [ cx (toString info.x), cy (toString info.y), r (toString info.r) ] []
