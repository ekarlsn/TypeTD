module EntityGen exposing
    ( EntityGen
    , Gen
    , ZombieId
    , create
    , fromRandom
    , make
    , map4
    , nextZombieIdGen
    , sequence
    , zombieIdEqual
    , zombieIdStr
    )

import Random


type ZombieId
    = ZombieId Int


type alias EntityGen =
    { randomSeed : Random.Seed
    , nextZombieId : ZombieId
    }


zombieIdStr : ZombieId -> String
zombieIdStr (ZombieId id) =
    String.fromInt id


nextZombieId : EntityGen -> ( ZombieId, EntityGen )
nextZombieId eg =
    ( eg.nextZombieId
    , { eg | nextZombieId = nextZombieId_ eg.nextZombieId }
    )


nextZombieIdGen : Gen ZombieId
nextZombieIdGen =
    makeGen nextZombieId


nextZombieId_ : ZombieId -> ZombieId
nextZombieId_ (ZombieId id) =
    ZombieId (id + 1)


make : Random.Seed -> EntityGen
make seed =
    { randomSeed = seed
    , nextZombieId = ZombieId 0
    }


zombieIdEqual : ZombieId -> ZombieId -> Bool
zombieIdEqual (ZombieId id1) (ZombieId id2) =
    id1 == id2


type Gen a
    = Gen (EntityGen -> ( a, EntityGen ))


constant : a -> Gen a
constant a =
    Gen (\eg -> ( a, eg ))


sequence : List (Gen a) -> Gen (List a)
sequence =
    List.foldr (map2 (::)) (constant [])


map2 : (a -> b -> c) -> Gen a -> Gen b -> Gen c
map2 f (Gen genA) (Gen genB) =
    Gen
        (\eg0 ->
            let
                ( a, eg1 ) =
                    genA eg0

                ( b, eg2 ) =
                    genB eg1
            in
            ( f a b, eg2 )
        )


map4 : (a -> b -> c -> d -> e) -> Gen a -> Gen b -> Gen c -> Gen d -> Gen e
map4 f (Gen genA) (Gen genB) (Gen genC) (Gen genD) =
    Gen
        (\eg0 ->
            let
                ( a, eg1 ) =
                    genA eg0

                ( b, eg2 ) =
                    genB eg1

                ( c, eg3 ) =
                    genC eg2

                ( d, eg4 ) =
                    genD eg3
            in
            ( f a b c d, eg4 )
        )


create : Gen a -> EntityGen -> ( a, EntityGen )
create (Gen gen) =
    gen


makeGen : (EntityGen -> ( a, EntityGen )) -> Gen a
makeGen f =
    Gen f


fromRandom : Random.Generator a -> Gen a
fromRandom randomGen =
    makeGen
        (\eg ->
            let
                ( val, newSeed ) =
                    Random.step randomGen eg.randomSeed
            in
            ( val, EntityGen newSeed eg.nextZombieId )
        )
