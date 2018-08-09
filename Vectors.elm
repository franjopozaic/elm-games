module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Random
import Time exposing (millisecond, second, Time)
import Debug
import VectorsModels exposing (..)
import VectorsGenerators exposing (generateObject)
import VectorsRender exposing (renderSvg, renderSettings)


type alias Model =
    { objects : List MovingObject
    , settings : Settings
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
            ( { model | objects = List.map (moveObject model.settings.isBounded) model.objects }, Cmd.none )

        AddObjectManual ->
            ( model, Random.generate AddObject (generateObject model.settings) )

        CheckObjects time ->
            let
                newObjects =
                    if (model.settings.isBounded) then
                        model.objects
                    else
                        model.objects
                            |> List.filter isInBounds
            in
                if ((List.length newObjects) < model.settings.maxNumberOfObjects) then
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
                        MaxNumberOfObjects ->
                            case settingValueInt of
                                Just value ->
                                    { settings | maxNumberOfObjects = value }

                                Nothing ->
                                    settings

                        IsBounded ->
                            { settings | isBounded = not settings.isBounded }

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (millisecond * 30) Tick
        , Time.every (millisecond * 10) CheckObjects
        ]


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "margin-left", "50px" ), ( "margin-top", "50px" ) ]
        ]
        [ renderSvg model.objects
        , div [] [ renderSettings model.settings ]
        ]


toBool : String -> Maybe Bool
toBool string =
    case String.toUpper string of
        "TRUE" ->
            Just True

        "FALSE" ->
            Just False

        _ ->
            Nothing


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


isInBounds : MovingObject -> Bool
isInBounds obj =
    case obj.object of
        Circle info ->
            (info.x > 0 && info.y > 0 && info.x < boxWidth && info.y < boxHeight)


moveObject : Bool -> MovingObject -> MovingObject
moveObject isBounded movingObject =
    case movingObject.object of
        Circle info ->
            case isBounded of
                True ->
                    case (isInBounds movingObject) of
                        True ->
                            { movingObject
                                | object =
                                    Circle (getCircleAtNextPosition (calculateOffset movingObject.gradient movingObject.velocity) info)
                            }

                        False ->
                            let
                                newGradient =
                                    -1 * movingObject.gradient

                                newVelocity =
                                    if (info.x == 0 || info.x == boxWidth) then
                                        -1 * movingObject.velocity
                                    else
                                        movingObject.velocity
                            in
                                { movingObject
                                    | object =
                                        Circle (getCircleAtNextPosition (calculateOffset newGradient newVelocity) info)
                                    , gradient = newGradient
                                    , velocity = newVelocity
                                }

                False ->
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
getCircleAtNextPosition ( xOffset, yOffset ) circle =
    { circle
        | x = boundNumber ( 0, boxWidth ) (circle.x + xOffset)
        , y = boundNumber ( 0, boxHeight ) (circle.y + yOffset)
    }


boundNumber : ( comparable, comparable ) -> comparable -> comparable
boundNumber ( min, max ) number =
    if (number < min) then
        min
    else if (number > max) then
        max
    else
        number
