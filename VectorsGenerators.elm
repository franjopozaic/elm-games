module VectorsGenerators exposing (..)

import VectorsModels exposing (..)
import Random


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
