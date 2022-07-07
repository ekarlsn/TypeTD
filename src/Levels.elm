module Levels exposing (CompletionStatus(..), Level, LevelInfo, LevelKey(..), LevelsWithProgress, SpawnZombieFunction, SpawnerParams, Wave, any, asList, completeCurrent, errorWave, getCurrentLevel, initialLevels, isNextUnlocked, nextLevel, tryStartLevel)

import EntityGen exposing (EntityGen)
import Gen.AssetPack as AssetPack exposing (AssetPack)
import List.Extra
import MapPoint exposing (MapPoint(..))
import Util
import WordList
import Zombie exposing (Zombie, ZombieTemplateDone)


type alias Level =
    { buildPoints : List ( String, MapPoint )
    , textureKey : AssetPack.TextureKey
    , waves : List Wave
    }


type alias Wave =
    { spawner : SpawnZombieFunction }


type alias SpawnZombieFunction =
    EntityGen -> AssetPack -> Float -> Float -> Maybe ( List Zombie, EntityGen )


type CompletionStatus
    = Completed
    | Unlocked
    | Locked


type LevelKey
    = Level1Key
    | Level2Key
    | Level3Key


type alias LevelInfo =
    { name : String
    , level : LevelKey
    , completionStatus : CompletionStatus
    }


type alias LevelsWithProgress =
    { before : List LevelInfo
    , current : LevelInfo
    , after : List LevelInfo
    }


isNextUnlocked : LevelsWithProgress -> Bool
isNextUnlocked levels =
    case levels.after of
        head :: _ ->
            isUnlocked head.completionStatus

        _ ->
            False


tryStartLevel : String -> LevelsWithProgress -> Maybe LevelsWithProgress
tryStartLevel levelName levels =
    let
        a : Maybe LevelInfo
        a =
            asList levels
                |> List.filter (.completionStatus >> isUnlocked)
                |> List.Extra.find (.name >> (==) levelName)
    in
    case a of
        Just levelInfo ->
            Just <| stepTo levelInfo.level levels

        Nothing ->
            Nothing


isUnlocked : CompletionStatus -> Bool
isUnlocked completionStatus =
    case completionStatus of
        Locked ->
            False

        Unlocked ->
            True

        Completed ->
            True


nextLevel : LevelsWithProgress -> Maybe LevelsWithProgress
nextLevel levels =
    case levels.after of
        h :: t ->
            Just
                { before = levels.current :: levels.before
                , current = h
                , after = t
                }

        [] ->
            Nothing


stepTo : LevelKey -> LevelsWithProgress -> LevelsWithProgress
stepTo levelKey levels =
    let
        before =
            List.Extra.takeWhile (.level >> (/=) levelKey) (asList levels)

        after =
            List.Extra.dropWhile (.level >> (/=) levelKey) (asList levels)
    in
    case ( before, after ) of
        ( _, h :: t ) ->
            { before = List.reverse before
            , current = h
            , after = t
            }

        _ ->
            -- Key not found, should be impossible
            levels


asList : LevelsWithProgress -> List LevelInfo
asList levels =
    List.reverse levels.before ++ [ levels.current ] ++ levels.after


getCurrentLevel : LevelsWithProgress -> Level
getCurrentLevel levels =
    getLevelFromKey levels.current.level


getLevelFromKey : LevelKey -> Level
getLevelFromKey levelKey =
    case levelKey of
        Level1Key ->
            level1

        Level2Key ->
            level2

        Level3Key ->
            level3


initialLevels : LevelsWithProgress
initialLevels =
    { before = []
    , current = { name = "Level1", level = Level1Key, completionStatus = Unlocked }
    , after =
        [ { name = "Level2", level = Level2Key, completionStatus = Locked }
        , { name = "Level3", level = Level3Key, completionStatus = Locked }
        ]
    }


any : (LevelInfo -> Bool) -> LevelsWithProgress -> Bool
any f levels =
    List.any f levels.before
        || f levels.current
        || List.any f levels.after


completeCurrent : LevelsWithProgress -> LevelsWithProgress
completeCurrent levels =
    -- Unlock next?
    let
        curr =
            levels.current
    in
    { levels
        | current = { curr | completionStatus = Completed }
        , after =
            listMapHead
                (\l ->
                    { l
                        | completionStatus =
                            case l.completionStatus of
                                Completed ->
                                    Completed

                                Unlocked ->
                                    Unlocked

                                Locked ->
                                    Unlocked
                    }
                )
                levels.after
    }


listMapHead : (a -> a) -> List a -> List a
listMapHead f list =
    case list of
        h :: t ->
            f h :: t

        [] ->
            []


type alias SpawnerParams =
    { numZombies : Int
    , durationBetweenZombies : Int
    }


introMapWayPoints : List MapPoint
introMapWayPoints =
    [ MapPoint 27.086907 31.947665
    , MapPoint 41.576513 39.524286
    , MapPoint 90.928746 39.708378
    , MapPoint 102.93105 30.999119
    , MapPoint 104.10081 17.853406
    , MapPoint 114.96732 8.6941778
    , MapPoint 131.2773 8.4639672
    , MapPoint 138.17876 19.949755
    , MapPoint 141.42493 34.036211
    , MapPoint 153.85099 40.062405
    , MapPoint 169.05858 43.371373
    , MapPoint 175.14419 55.263252
    , MapPoint 171.49172 65.444801
    , MapPoint 160.87833 72.239043
    , MapPoint 125.54424 71.615932
    , MapPoint 112.95314 78.599208
    , MapPoint 62.142874 79.029262
    , MapPoint 54.325363 81.614826
    , MapPoint 50.811043 88.341955
    , MapPoint 50.406274 99.794872
    ]


introMapStartPoint : MapPoint
introMapStartPoint =
    MapPoint 0 32


wayPointForLevel : LevelKey -> ( MapPoint, List MapPoint )
wayPointForLevel level =
    case level of
        Level1Key ->
            ( introMapStartPoint, introMapWayPoints )

        Level2Key ->
            ( level2StartPoint, level2WayPoints )

        Level3Key ->
            ( level3StartPoint, level3WayPoints )


introZombie : LevelKey -> ZombieTemplateDone
introZombie level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Troll
        |> Zombie.withSpeed 10
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList


animalZombie : LevelKey -> ZombieTemplateDone
animalZombie level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie2
        |> Zombie.withSpeed 19
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList


slowShieldedRunner : LevelKey -> ZombieTemplateDone
slowShieldedRunner level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie7
        |> Zombie.withSpeed 14
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList
        |> Zombie.withEffects [ Zombie.Shielded ]
        |> Zombie.withDelayedEffects [ Zombie.pendingRemoveEffect Zombie.Shielded 3000 ]


quizzRunner : LevelKey -> ZombieTemplateDone
quizzRunner level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie3
        |> Zombie.withSpeed 15
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList
        |> Zombie.withEffects [ Zombie.Blanks 4 ]


shieldedRunner : LevelKey -> ZombieTemplateDone
shieldedRunner level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie4
        |> Zombie.withSpeed 25
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList
        |> Zombie.withEffects [ Zombie.Shielded ]
        |> Zombie.withDelayedEffects [ Zombie.pendingRemoveEffect Zombie.Shielded 3000 ]


fastShieldAndBlankRunner : LevelKey -> ZombieTemplateDone
fastShieldAndBlankRunner level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie5
        |> Zombie.withSpeed 30
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList
        |> Zombie.withEffects [ Zombie.Shielded, Zombie.Blanks 4 ]
        |> Zombie.withDelayedEffects
            [ Zombie.pendingRemoveEffect Zombie.Shielded 3000
            ]


slowHardBlanksRunner : LevelKey -> ZombieTemplateDone
slowHardBlanksRunner level =
    Zombie.basicZombieTemplate
        |> Zombie.withTexture AssetPack.Zombie6
        |> Zombie.withSpeed 7
        |> Zombie.withWayPoints (wayPointForLevel level)
        |> Zombie.withWords WordList.wordList
        |> Zombie.withEffects [ Zombie.Blanks 2 ]
        |> Zombie.withDelayedEffects
            [ Zombie.DelayedEffect
                { delay = 7000
                , effect = Zombie.removeEffect (Zombie.Blanks 2) >> Zombie.applyEffect (Zombie.Blanks 3)
                }
            , Zombie.DelayedEffect
                { delay = 14000
                , effect = Zombie.removeEffect (Zombie.Blanks 3) >> Zombie.applyEffect (Zombie.Blanks 4)
                }
            , Zombie.DelayedEffect
                { delay = 24000
                , effect = Zombie.removeEffect (Zombie.Blanks 4) >> Zombie.applyEffect (Zombie.Blanks 5)
                }
            ]


level2StartPoint : MapPoint
level2StartPoint =
    MapPoint 0 80


level2WayPoints : List MapPoint
level2WayPoints =
    [ MapPoint 53.907444 79.997353
    , MapPoint 61.097612 77.01049
    , MapPoint 67.141874 69.71847
    , MapPoint 67.620939 59.31583
    , MapPoint 72.613962 52.993955
    , MapPoint 81.821967 48.951703
    , MapPoint 89.984654 48.488692
    , MapPoint 98.405061 51.668
    , MapPoint 103.35839 57.36952
    , MapPoint 113.61626 56.332115
    , MapPoint 121.27774 52.831325
    , MapPoint 126.91961 45.629618
    , MapPoint 127.5938 35.588675
    , MapPoint 133.0653 27.939197
    , MapPoint 142.91755 23.830841
    , MapPoint 176.48805 24.069005
    , MapPoint 184.30608 28.383495
    , MapPoint 191.90187 31.958175
    , MapPoint 199.90717 32.066815
    ]


level2BuildPoints : List ( String, MapPoint )
level2BuildPoints =
    [ ( "Here", MapPoint 16.300767 92.74814 )
    , ( "There", MapPoint 58.384935 92.834335 )
    , ( "Hillside", MapPoint 85.27328 62.185997 )
    , ( "Topview", MapPoint 108.86603 41.61942 )
    , ( "Lookout", MapPoint 124.75259 65.78394 )
    , ( "Center", MapPoint 148.07096 37.087395 )
    , ( "Outskirt", MapPoint 181.79945 44.626385 )
    , ( "Homebound", MapPoint 136.3059 93.101457 )
    ]


level3StartPoint : MapPoint
level3StartPoint =
    MapPoint 26 0


level3WayPoints : List MapPoint
level3WayPoints =
    [ MapPoint 32.3155 10.245197
    , MapPoint 43.641345 13.402535
    , MapPoint 55.966061 17.263537
    , MapPoint 60.008522 26.215345
    , MapPoint 60.185567 35.816531
    , MapPoint 67.664192 41.233576
    , MapPoint 76.210097 43.875403
    , MapPoint 87.330241 45.809894
    , MapPoint 94.376286 51.750739
    , MapPoint 97.042487 61.483726
    , MapPoint 99.985937 68.8291
    , MapPoint 108.19427 75.318841
    , MapPoint 124.77932 75.77659
    , MapPoint 144.74981 76.378908
    , MapPoint 150.56739 78.802104
    , MapPoint 159.31379 83.291148
    , MapPoint 180.13192 83.403501
    , MapPoint 200.04247 83.081412
    ]


level3BuildPoints : List ( String, MapPoint )
level3BuildPoints =
    [ ( "Top", MapPoint 35.053438 25.223374 )
    , ( "Center", MapPoint 100.93716 35.26628 )
    , ( "Apex", MapPoint 55.840116 53.364845 )
    , ( "Corner", MapPoint 118.24815 63.191688 )
    ]


level1BuildPoints : List ( String, MapPoint )
level1BuildPoints =
    [ ( "Start", MapPoint 16.546458 21.452409 )
    , ( "End", MapPoint 33.985299 67.435222 )
    , ( "Corner", MapPoint 79.700435 27.983536 )
    , ( "Point", MapPoint 120.91773 25.989522 )
    , ( "Shadows", MapPoint 157.93286 28.583941 )
    , ( "Apex", MapPoint 152.24013 57.690189 )
    , ( "Bump", MapPoint 132.11031 86.240222 )
    , ( "Finisher", MapPoint 71.091339 93.749813 )
    , ( "Center", MapPoint 104.12157 57.429729 )
    ]


level1 : Level
level1 =
    { buildPoints = level1BuildPoints
    , textureKey = AssetPack.Map1
    , waves =
        [ -- Wave 1
          { spawner =
                spawnZombiesEvenly
                    (introZombie Level1Key)
                    { durationBetweenZombies = 2100, numZombies = 8 }
          }

        -- Wave 2
        , { spawner =
                spawnZombiesEvenly
                    (slowShieldedRunner Level1Key)
                    { durationBetweenZombies = 2100, numZombies = 16 }
          }

        -- Wave 3
        , { spawner =
                spawnZombiesEvenly
                    (quizzRunner Level1Key)
                    { durationBetweenZombies = 2100, numZombies = 20 }
          }
        ]
    }


listReverseMap : a -> List (a -> b) -> List b
listReverseMap value =
    List.map (\f -> f value)


level2 : Level
level2 =
    { buildPoints = level2BuildPoints
    , textureKey = AssetPack.Map2
    , waves =
        [ -- Wave 1
          { spawner =
                spawnZombiesRoundRobin
                    (listReverseMap Level2Key
                        [ animalZombie
                        , introZombie
                        , introZombie
                        , shieldedRunner
                        ]
                    )
                    { durationBetweenZombies = 1500
                    , numZombies = 10
                    }
          }

        -- Wave 2
        , { spawner =
                spawnZombiesRoundRobin
                    [ animalZombie Level2Key
                    , shieldedRunner Level2Key
                    , quizzRunner Level2Key
                    ]
                    { durationBetweenZombies = 1200
                    , numZombies = 20
                    }
          }

        -- Wave 3
        , { spawner =
                spawnZombiesRoundRobin
                    [ shieldedRunner Level2Key
                    , quizzRunner Level2Key
                    ]
                    { durationBetweenZombies = 1200
                    , numZombies = 40
                    }
          }

        -- Wave 4
        , { spawner =
                spawnZombiesRoundRobin
                    [ shieldedRunner Level2Key
                    , quizzRunner Level2Key
                    ]
                    { durationBetweenZombies = 800
                    , numZombies = 15
                    }
          }

        -- Wave 5
        , { spawner =
                spawnZombiesRoundRobin
                    [ slowHardBlanksRunner Level2Key
                    , fastShieldAndBlankRunner Level2Key
                    ]
                    { durationBetweenZombies = 2000
                    , numZombies = 15
                    }
          }
        ]
    }


level3 : Level
level3 =
    { buildPoints = level3BuildPoints
    , textureKey = AssetPack.Map3
    , waves =
        [ -- Wave 1
          { spawner =
                spawnZombiesEvenly
                    (animalZombie Level3Key)
                    { durationBetweenZombies = 600, numZombies = 10 }
          }

        -- Wave 2
        , { spawner =
                spawnZombiesRoundRobin
                    [ animalZombie Level3Key
                    , animalZombie Level3Key
                    , introZombie Level3Key
                    ]
                    { durationBetweenZombies = 600, numZombies = 20 }
          }

        -- Wave 3
        , { spawner =
                spawnZombiesRoundRobin
                    [ slowHardBlanksRunner Level3Key
                    ]
                    { durationBetweenZombies = 2000, numZombies = 20 }
          }

        -- Wave 3
        , { spawner =
                spawnZombiesRoundRobin
                    [ animalZombie Level3Key
                    , slowHardBlanksRunner Level3Key
                    , slowHardBlanksRunner Level3Key
                    , slowHardBlanksRunner Level3Key
                    ]
                    { durationBetweenZombies = 1300, numZombies = 50 }
          }

        -- Wave 4
        , { spawner =
                spawnZombiesRoundRobin
                    [ introZombie Level3Key
                    , slowHardBlanksRunner Level3Key
                    , shieldedRunner Level3Key
                    ]
                    { durationBetweenZombies = 1300, numZombies = 50 }
          }
        ]
    }


spawnZombiesFromList : List ZombieTemplateDone -> { durationBetweenZombies : Int } -> EntityGen -> AssetPack -> Float -> Float -> Maybe ( List Zombie, EntityGen )
spawnZombiesFromList zTemplates { durationBetweenZombies } eg0 ap timePrev timeNow =
    if List.length zTemplates * durationBetweenZombies + durationBetweenZombies // 2 < round timeNow then
        Nothing

    else
        let
            numZombiesBefore : Int
            numZombiesBefore =
                ceiling (timePrev / toFloat durationBetweenZombies)

            numZombiesAfter : Int
            numZombiesAfter =
                ceiling (timeNow / toFloat durationBetweenZombies)
        in
        Just
            (zTemplates
                |> List.take numZombiesAfter
                |> List.drop numZombiesBefore
                |> List.map (Zombie.makeZombieGen ap)
                |> EntityGen.sequence
                |> Util.flip EntityGen.create eg0
            )


spawnZombiesRoundRobin : List ZombieTemplateDone -> SpawnerParams -> EntityGen -> AssetPack -> Float -> Float -> Maybe ( List Zombie, EntityGen )
spawnZombiesRoundRobin zTemplates { numZombies, durationBetweenZombies } =
    spawnZombiesFromList
        (List.Extra.cycle numZombies zTemplates)
        { durationBetweenZombies = durationBetweenZombies }


spawnZombiesEvenly : ZombieTemplateDone -> SpawnerParams -> EntityGen -> AssetPack -> Float -> Float -> Maybe ( List Zombie, EntityGen )
spawnZombiesEvenly zTemplate params =
    spawnZombiesFromList
        (List.repeat params.numZombies zTemplate)
        { durationBetweenZombies = params.durationBetweenZombies }


errorWave : Wave
errorWave =
    { spawner = errorSpawner }


errorSpawner : EntityGen -> AssetPack -> Float -> Float -> Maybe ( List Zombie, EntityGen )
errorSpawner _ _ _ _ =
    Nothing
