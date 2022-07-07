module MapPoint exposing
    ( MapPoint(..)
    , RightLeft(..)
    , distance
    , fuzz
    , fuzzGen
    , isWithin
    , moveTowards
    , moveTowardsWithAngle
    , rightOf
    , toRenderPoint
    , toString
    )

import EntityGen
import Math.Vector2 as Vec2
import Random


type MapPoint
    = MapPoint Float Float


toRenderPoint : Float -> Float -> MapPoint -> ( Float, Float )
toRenderPoint scaleX scaleY mp =
    case mp of
        MapPoint x y ->
            ( x * scaleX, y * scaleY )


isWithin : Float -> MapPoint -> MapPoint -> Bool
isWithin margin a b =
    Vec2.length (Vec2.sub (toVec2 a) (toVec2 b)) < margin


toVec2 : MapPoint -> Vec2.Vec2
toVec2 mp =
    case mp of
        MapPoint x y ->
            Vec2.vec2 x y


moveTowardsWithAngle : Float -> Float -> MapPoint -> MapPoint -> ( MapPoint, Float )
moveTowardsWithAngle speed dt from to =
    let
        source =
            toVec2 from

        dest =
            toVec2 to

        travel =
            Vec2.direction dest source
                |> Vec2.scale ((speed / 1000) * dt)

        res =
            Vec2.add source travel

        angle =
            atan2 (Vec2.getY travel) (Vec2.getX travel)
    in
    ( MapPoint (Vec2.getX res) (Vec2.getY res), angle )


moveTowards : Float -> Float -> MapPoint -> MapPoint -> MapPoint
moveTowards speed dt from to =
    let
        v1 =
            toVec2 from

        v2 =
            toVec2 to

        vec =
            Vec2.direction v2 v1
                |> Vec2.scale ((speed / 1000) * dt)

        res =
            Vec2.add v1 vec
    in
    MapPoint (Vec2.getX res) (Vec2.getY res)


toString : MapPoint -> String
toString mp =
    case mp of
        MapPoint x y ->
            "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


distance : MapPoint -> MapPoint -> Float
distance p1 p2 =
    Vec2.distance (toVec2 p1) (toVec2 p2)


type RightLeft
    = Right
    | Left


rightOf : MapPoint -> MapPoint -> Bool
rightOf (MapPoint x1 _) (MapPoint x2 _) =
    x1 < x2


fuzz : Float -> MapPoint -> Random.Generator MapPoint
fuzz fuzzFactor (MapPoint x y) =
    Random.map2
        MapPoint
        (Random.float (x - fuzzFactor) (x + fuzzFactor))
        (Random.float (y - fuzzFactor) (y + fuzzFactor))


fuzzGen : Float -> MapPoint -> EntityGen.Gen MapPoint
fuzzGen fuzzFactor mapPoint =
    fuzz fuzzFactor mapPoint
        |> EntityGen.fromRandom
