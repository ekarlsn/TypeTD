module Gen.AssetPack exposing (AssetPack, AssetPackBuilder, AssetPackContainer, LoadAudio(..), LoadTexture(..), SoundKey(..), TextureKey(..), getSound, getTexture, initialAssetPackBuilder, insertSound, insertTexture, loadAllSounds, loadAllTextures, tryBuildAssetPack)

import Audio
import Canvas.Texture exposing (Texture)


type TextureKey
    = Troll
    | Zombie2
    | Zombie3
    | Zombie4
    | Zombie5
    | Zombie6
    | Zombie7
    | Zombie8
    | Zombie9
    | Zombie10
    | Map1
    | Map2
    | Map3
    | TowerFreezer
    | TowerSimplifier
    | Arrow
    | MainMenu
    | SelectLevel
    | LevelFinished
    | InfoBox
    | Ui
    | HelpScreen
    | TextShield


type SoundKey
    = MainMenuBgSound
    | LevelFailed
    | LevelSuccess
    | ZombieDie
    | FreezerFire
    | FreezerHit


type alias AssetPackContainer textureType soundType =
    { troll : textureType
    , zombie2 : textureType
    , zombie3 : textureType
    , zombie4 : textureType
    , zombie5 : textureType
    , zombie6 : textureType
    , zombie7 : textureType
    , zombie8 : textureType
    , zombie9 : textureType
    , zombie10 : textureType
    , map1 : textureType
    , map2 : textureType
    , map3 : textureType
    , towerFreezer : textureType
    , towerSimplifier : textureType
    , arrow : textureType
    , mainMenu : textureType
    , selectLevel : textureType
    , levelFinished : textureType
    , infoBox : textureType
    , ui : textureType
    , helpScreen : textureType
    , textShield : textureType
    , mainMenuBgSound : soundType
    , levelFailed : soundType
    , levelSuccess : soundType
    , zombieDie : soundType
    , freezerFire : soundType
    , freezerHit : soundType
    }


type alias AssetPack =
    AssetPackContainer Texture Audio.Source


type alias AssetPackBuilder =
    AssetPackContainer (Maybe Texture) (Maybe Audio.Source)


initialAssetPackBuilder : AssetPackBuilder
initialAssetPackBuilder =
    { troll = Nothing
    , zombie2 = Nothing
    , zombie3 = Nothing
    , zombie4 = Nothing
    , zombie5 = Nothing
    , zombie6 = Nothing
    , zombie7 = Nothing
    , zombie8 = Nothing
    , zombie9 = Nothing
    , zombie10 = Nothing
    , map1 = Nothing
    , map2 = Nothing
    , map3 = Nothing
    , towerFreezer = Nothing
    , towerSimplifier = Nothing
    , arrow = Nothing
    , mainMenu = Nothing
    , selectLevel = Nothing
    , levelFinished = Nothing
    , infoBox = Nothing
    , ui = Nothing
    , helpScreen = Nothing
    , textShield = Nothing
    , mainMenuBgSound = Nothing
    , levelFailed = Nothing
    , levelSuccess = Nothing
    , zombieDie = Nothing
    , freezerFire = Nothing
    , freezerHit = Nothing
    }


tryBuildAssetPack : AssetPackBuilder -> Maybe AssetPack
tryBuildAssetPack tpb =
    case
        ( [ tpb.troll
          , tpb.zombie2
          , tpb.zombie3
          , tpb.zombie4
          , tpb.zombie5
          , tpb.zombie6
          , tpb.zombie7
          , tpb.zombie8
          , tpb.zombie9
          , tpb.zombie10
          , tpb.map1
          , tpb.map2
          , tpb.map3
          , tpb.towerFreezer
          , tpb.towerSimplifier
          , tpb.arrow
          , tpb.mainMenu
          , tpb.selectLevel
          , tpb.levelFinished
          , tpb.infoBox
          , tpb.ui
          , tpb.helpScreen
          , tpb.textShield
          ]
        , [ tpb.mainMenuBgSound
          , tpb.levelFailed
          , tpb.levelSuccess
          , tpb.zombieDie
          , tpb.freezerFire
          , tpb.freezerHit
          ]
        )
    of
        ( [ Just t0, Just t1, Just t2, Just t3, Just t4, Just t5, Just t6, Just t7, Just t8, Just t9, Just t10, Just t11, Just t12, Just t13, Just t14, Just t15, Just t16, Just t17, Just t18, Just t19, Just t20, Just t21, Just t22 ], [ Just t23, Just t24, Just t25, Just t26, Just t27, Just t28 ] ) ->
            Just <|
                { troll = t0
                , zombie2 = t1
                , zombie3 = t2
                , zombie4 = t3
                , zombie5 = t4
                , zombie6 = t5
                , zombie7 = t6
                , zombie8 = t7
                , zombie9 = t8
                , zombie10 = t9
                , map1 = t10
                , map2 = t11
                , map3 = t12
                , towerFreezer = t13
                , towerSimplifier = t14
                , arrow = t15
                , mainMenu = t16
                , selectLevel = t17
                , levelFinished = t18
                , infoBox = t19
                , ui = t20
                , helpScreen = t21
                , textShield = t22
                , mainMenuBgSound = t23
                , levelFailed = t24
                , levelSuccess = t25
                , zombieDie = t26
                , freezerFire = t27
                , freezerHit = t28
                }

        _ ->
            Nothing


insertTexture : TextureKey -> Texture -> AssetPackBuilder -> AssetPackBuilder
insertTexture id t tpb =
    case id of
        Troll ->
            { tpb | troll = Just t }

        Zombie2 ->
            { tpb | zombie2 = Just t }

        Zombie3 ->
            { tpb | zombie3 = Just t }

        Zombie4 ->
            { tpb | zombie4 = Just t }

        Zombie5 ->
            { tpb | zombie5 = Just t }

        Zombie6 ->
            { tpb | zombie6 = Just t }

        Zombie7 ->
            { tpb | zombie7 = Just t }

        Zombie8 ->
            { tpb | zombie8 = Just t }

        Zombie9 ->
            { tpb | zombie9 = Just t }

        Zombie10 ->
            { tpb | zombie10 = Just t }

        Map1 ->
            { tpb | map1 = Just t }

        Map2 ->
            { tpb | map2 = Just t }

        Map3 ->
            { tpb | map3 = Just t }

        TowerFreezer ->
            { tpb | towerFreezer = Just t }

        TowerSimplifier ->
            { tpb | towerSimplifier = Just t }

        Arrow ->
            { tpb | arrow = Just t }

        MainMenu ->
            { tpb | mainMenu = Just t }

        SelectLevel ->
            { tpb | selectLevel = Just t }

        LevelFinished ->
            { tpb | levelFinished = Just t }

        InfoBox ->
            { tpb | infoBox = Just t }

        Ui ->
            { tpb | ui = Just t }

        HelpScreen ->
            { tpb | helpScreen = Just t }

        TextShield ->
            { tpb | textShield = Just t }


insertSound : SoundKey -> Audio.Source -> AssetPackBuilder -> AssetPackBuilder
insertSound id sound apb =
    case id of
        MainMenuBgSound ->
            { apb | mainMenuBgSound = Just sound }

        LevelFailed ->
            { apb | levelFailed = Just sound }

        LevelSuccess ->
            { apb | levelSuccess = Just sound }

        ZombieDie ->
            { apb | zombieDie = Just sound }

        FreezerFire ->
            { apb | freezerFire = Just sound }

        FreezerHit ->
            { apb | freezerHit = Just sound }


type LoadTexture
    = LoadTexture String TextureKey


loadAllTextures : List LoadTexture
loadAllTextures =
    [ ( "monster/zombie_1", Troll )
    , ( "monster/zombie_2", Zombie2 )
    , ( "monster/zombie_3", Zombie3 )
    , ( "monster/zombie_4", Zombie4 )
    , ( "monster/zombie_5", Zombie5 )
    , ( "monster/zombie_6", Zombie6 )
    , ( "monster/zombie_7", Zombie7 )
    , ( "monster/zombie_8", Zombie8 )
    , ( "monster/zombie_9", Zombie9 )
    , ( "monster/zombie_10", Zombie10 )
    , ( "map/map1", Map1 )
    , ( "map/map2", Map2 )
    , ( "map/map3", Map3 )
    , ( "tower/archer", TowerFreezer )
    , ( "tower/basic", TowerSimplifier )
    , ( "tower/arrow", Arrow )
    , ( "ui/main_menu", MainMenu )
    , ( "ui/select_level", SelectLevel )
    , ( "ui/level_finished", LevelFinished )
    , ( "ui/infobox", InfoBox )
    , ( "ui/ui", Ui )
    , ( "ui/help_screen", HelpScreen )
    , ( "text_shield", TextShield )
    ]
        |> List.map (\( name, key ) -> LoadTexture ("./assets/img/" ++ name ++ ".png") key)


type LoadAudio
    = LoadAudio SoundKey String


loadAllSounds : List LoadAudio
loadAllSounds =
    [ ( MainMenuBgSound, "wakka" )
    , ( LevelFailed, "level_failed" )
    , ( LevelSuccess, "level_success" )
    , ( ZombieDie, "zombie_die" )
    , ( FreezerFire, "freezer_fire" )
    , ( FreezerHit, "freezer_hit" )
    ]
        |> List.map (\( key, filename ) -> LoadAudio key ("./assets/sound/" ++ filename ++ ".mp3"))


getTexture : AssetPack -> TextureKey -> Texture
getTexture tp id =
    case id of
        Troll ->
            tp.troll

        Zombie2 ->
            tp.zombie2

        Zombie3 ->
            tp.zombie3

        Zombie4 ->
            tp.zombie4

        Zombie5 ->
            tp.zombie5

        Zombie6 ->
            tp.zombie6

        Zombie7 ->
            tp.zombie7

        Zombie8 ->
            tp.zombie8

        Zombie9 ->
            tp.zombie9

        Zombie10 ->
            tp.zombie10

        Map1 ->
            tp.map1

        Map2 ->
            tp.map2

        Map3 ->
            tp.map3

        TowerFreezer ->
            tp.towerFreezer

        TowerSimplifier ->
            tp.towerSimplifier

        Arrow ->
            tp.arrow

        MainMenu ->
            tp.mainMenu

        SelectLevel ->
            tp.selectLevel

        LevelFinished ->
            tp.levelFinished

        InfoBox ->
            tp.infoBox

        Ui ->
            tp.ui

        HelpScreen ->
            tp.helpScreen

        TextShield ->
            tp.textShield


getSound : SoundKey -> AssetPack -> Audio.Source
getSound key soundPack =
    case key of
        MainMenuBgSound ->
            soundPack.mainMenuBgSound

        LevelFailed ->
            soundPack.levelFailed

        LevelSuccess ->
            soundPack.levelSuccess

        ZombieDie ->
            soundPack.zombieDie

        FreezerFire ->
            soundPack.freezerFire

        FreezerHit ->
            soundPack.freezerHit
