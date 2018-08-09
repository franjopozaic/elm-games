module VectorsModels exposing (..)

import Time exposing (millisecond, second, Time)


type Msg
    = Tick Time
    | CheckObjects Time
    | AddObject MovingObject
    | AddObjectManual
    | SettingsChange ChangeType String


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
    , maxNumberOfObjects : Int
    , isBounded : Bool
    }


type alias IntRange =
    { min : Int
    , max : Int
    }


type alias FloatRange =
    { min : Float
    , max : Float
    }


type alias CircleInfo =
    { x : Int
    , y : Int
    , r : Int
    }


type ChangeType
    = MinSize
    | MaxSize
    | MinVelocity
    | MaxVelocity
    | MinGradient
    | MaxGradient
    | MaxNumberOfObjects
    | IsBounded


initMovingObjects : List MovingObject
initMovingObjects =
    []


initialSettings : Settings
initialSettings =
    { sizeRange = { min = 1, max = 3 }
    , velocityRange = { min = 1, max = 30 }
    , gradientRange = { min = -3.0, max = 3.0 }
    , maxNumberOfObjects = 4
    , isBounded = False
    }


boxWidth : Int
boxWidth =
    620


boxHeight : Int
boxHeight =
    620
