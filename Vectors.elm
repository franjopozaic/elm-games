module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (millisecond, second, Time)
import Debug


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias CircleInfo =
    { x : Int
    , y : Int
    , r : Int
    }


type alias PointInfo =
    { x : Int
    , y : Int
    }


type alias Model =
    { objects : List MovingObject
    , settings : Settings
    }


type Object
    = Circle CircleInfo


type alias MovingObject =
    { object : Object
    , gradient : Float
    , velocity : Int
    }


type alias Settings =
    { sizeRange : IntRange
    , gradientRange : FloatRange
    , velocityRange : IntRange
    }


type alias IntRange =
    { min : Int
    , max : Int
    }


type alias FloatRange =
    { min : Float
    , max : Float
    }


type ChangeType
    = MinSize
    | MaxSize
    | MinVelocity
    | MaxVelocity
    | MinGradient
    | MaxGradient


type Msg
    = Tick Time
    | CheckObjects Time
    | AddObject MovingObject
    | SettingsChange ChangeType String


initMovingObjects : List MovingObject
initMovingObjects =
    []


initialSettings : Settings
initialSettings =
    { sizeRange = { min = 1, max = 3 }
    , velocityRange = { min = 1, max = 30 }
    , gradientRange = { min = -3.0, max = 3.0 }
    }


init : ( Model, Cmd Msg )
init =
    ( Model initMovingObjects initialSettings
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | objects = List.map moveObject model.objects }, Cmd.none )

        CheckObjects time ->
            let
                newObjects =
                    model.objects
                        |> List.filter isInBounds
            in
                if ((List.length newObjects) < 4000) then
                    ( { model | objects = newObjects }, Random.generate AddObject (generateObject model.settings) )
                else
                    ( { model | objects = newObjects }, Cmd.none )

        AddObject obj ->
            ( { model | objects = obj :: model.objects }, Cmd.none )

        SettingsChange changeType value ->
            let
                settings =
                    model.settings

                settingValueInt =
                    value
                        |> String.toInt
                        |> toMaybe

                settingValueFloat =
                    value
                        |> String.toFloat
                        |> toMaybe

                newSettings =
                    case changeType of
                        MinSize ->
                            { settings | sizeRange = updateIntRange ( settingValueInt, Nothing ) settings.sizeRange }

                        MaxSize ->
                            { settings | sizeRange = updateIntRange ( Nothing, settingValueInt ) settings.sizeRange }

                        MinVelocity ->
                            { settings | velocityRange = updateIntRange ( settingValueInt, Nothing ) settings.velocityRange }

                        MaxVelocity ->
                            { settings | velocityRange = updateIntRange ( Nothing, settingValueInt ) settings.velocityRange }

                        MinGradient ->
                            { settings | gradientRange = updateFloatRange ( settingValueFloat, Nothing ) settings.gradientRange }

                        MaxGradient ->
                            { settings | gradientRange = updateFloatRange ( Nothing, settingValueFloat ) settings.gradientRange }
            in
                ( { model | settings = newSettings }, Cmd.none )


toMaybe : Result String a -> Maybe a
toMaybe result =
    case result of
        Ok value ->
            Just value

        Err _ ->
            Nothing


updateFloatRange : ( Maybe Float, Maybe Float ) -> FloatRange -> FloatRange
updateFloatRange ( newMin, newMax ) oldRange =
    case ( newMin, newMax ) of
        ( Just minValue, Just maxValue ) ->
            { min = minValue, max = maxValue }

        ( Just minValue, Nothing ) ->
            { min = minValue, max = oldRange.max }

        ( Nothing, Just maxValue ) ->
            { min = oldRange.min, max = maxValue }

        ( Nothing, Nothing ) ->
            { min = oldRange.min, max = oldRange.max }


updateIntRange : ( Maybe Int, Maybe Int ) -> IntRange -> IntRange
updateIntRange ( newMin, newMax ) oldRange =
    case ( newMin, newMax ) of
        ( Just minValue, Just maxValue ) ->
            { min = minValue, max = maxValue }

        ( Just minValue, Nothing ) ->
            { min = minValue, max = oldRange.max }

        ( Nothing, Just maxValue ) ->
            { min = oldRange.min, max = maxValue }

        ( Nothing, Nothing ) ->
            { min = oldRange.min, max = oldRange.max }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (millisecond * 30) Tick
        , Time.every (millisecond * 10) CheckObjects
        ]


generateObject : Settings -> Random.Generator MovingObject
generateObject settings =
    Random.map4 createMovingObject
        randomPoint
        (randomGradient settings.gradientRange)
        (randomVelocity settings.velocityRange)
        (randomSize settings.sizeRange)


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.int 1 4
        |> Random.andThen
            (\v ->
                case v of
                    1 ->
                        randomPointOnLeftWall

                    2 ->
                        randomPointOnRightWall

                    3 ->
                        randomPointOnTopWall

                    4 ->
                        randomPointOnBottomWall

                    _ ->
                        randomPointOnBottomWall
            )


randomPointOnLeftWall : Random.Generator ( Int, Int )
randomPointOnLeftWall =
    Random.map (\y -> ( 0, y )) (Random.int 0 boxHeight)


randomPointOnRightWall : Random.Generator ( Int, Int )
randomPointOnRightWall =
    Random.map (\y -> ( boxWidth, y )) (Random.int 0 boxHeight)


randomPointOnTopWall : Random.Generator ( Int, Int )
randomPointOnTopWall =
    Random.map (\x -> ( x, 0 )) (Random.int 0 boxHeight)


randomPointOnBottomWall : Random.Generator ( Int, Int )
randomPointOnBottomWall =
    Random.map (\x -> ( x, boxHeight )) (Random.int 0 boxHeight)


randomVelocity : IntRange -> Random.Generator Int
randomVelocity range =
    Random.int range.min range.max


randomSize : IntRange -> Random.Generator Int
randomSize range =
    Random.int range.min range.max


randomGradient : FloatRange -> Random.Generator Float
randomGradient range =
    Random.float range.min range.max


createMovingObject : ( Int, Int ) -> Float -> Int -> Int -> MovingObject
createMovingObject ( x, y ) gradient velocity size =
    MovingObject (Circle (CircleInfo x y size)) gradient velocity


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "margin-left", "50px" ), ( "margin-top", "50px" ) ]
        ]
        [ renderSvg model.objects
        , div [] [ renderSettings model.settings ]
        ]


renderSettings : Settings -> Html Msg
renderSettings settings =
    Html.form []
        [ fieldset []
            [ div []
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


renderSvg : List { a | object : Object } -> Html Msg
renderSvg objects =
    svg
        [ Svg.Attributes.width (toString boxWidth)
        , Svg.Attributes.height (toString boxHeight)
        , viewBox ("40 40 " ++ (toString (boxWidth - 40)) ++ " " ++ (toString (boxHeight - 40)))
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


isInBounds : MovingObject -> Bool
isInBounds obj =
    case obj.object of
        Circle info ->
            not (info.x < -40 || info.y < -40 || info.x > boxHeight + 40 || info.y > boxWidth + 40)


moveObject : MovingObject -> MovingObject
moveObject movingObject =
    case movingObject.object of
        Circle info ->
            { movingObject
                | object =
                    Circle (getCircleAtNextPosition (calculateOffset movingObject.gradient movingObject.velocity) info)
            }


calculateOffset : Float -> Int -> ( Int, Int )
calculateOffset gradient velocity =
    let
        multiplier =
            if (velocity > 0) then
                1
            else
                -1

        x =
            sqrt (abs (toFloat velocity) / (1 + gradient * gradient))
    in
        ( round (x * multiplier), round ((gradient * x) * multiplier) )


getCircleAtNextPosition : ( Int, Int ) -> CircleInfo -> CircleInfo
getCircleAtNextPosition ( xOffset, yOffset ) rect =
    { rect
        | x = rect.x + xOffset
        , y = rect.y + yOffset
    }


boxWidth : Int
boxWidth =
    620


boxHeight : Int
boxHeight =
    620
