port module Main exposing (Model, Msg, main)

import Audio
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Canvas exposing (..)
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (font)
import Canvas.Texture exposing (Texture)
import Color
import Css
import Element exposing (Element)
import EntityGen exposing (EntityGen, ZombieId)
import Gen.AssetPack as AssetPack exposing (AssetPack, AssetPackBuilder, SoundKey, TextureKey, initialAssetPackBuilder, tryBuildAssetPack)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Styled
import Html.Styled.Attributes
import Json.Decode
import Json.Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Levels exposing (Level, LevelsWithProgress, Wave, initialLevels)
import List
import List.Extra
import MapPoint exposing (MapPoint)
import Menu exposing (TowerType(..))
import Money exposing (Money)
import Projectile exposing (Projectile)
import Random
import Sounds exposing (SoundPack)
import Task
import Time
import Tower exposing (Tower)
import TypedData exposing (TypedData)
import Util
import Zombie exposing (Zombie, renderTroll, updateTrollAnim)


type alias RunningData =
    { time : Float
    , zombies : List Zombie
    , stopSpawning : Bool
    , currentWave : Wave
    , remainingWaves : List Wave
    , projectiles : List Projectile
    , shared : SharedRunningData
    }


type alias BetweenWavesData =
    { time : Float
    , nextWave : Wave
    , remainingWaves : List Wave
    , shared : SharedRunningData
    }


type alias SharedRunningData =
    { typedData : TypedData
    , allLevels : LevelsWithProgress
    , entityGen : EntityGen
    , assetPack : AssetPack
    , soundPack : SoundPack
    , buildSm : Menu.Selection
    , freeBuildPoints : List ( String, MapPoint )
    , towers : List Tower
    , timeSkew : Float
    , globalDelayedEffects : List GlobalEffectDelayed
    , money : Money
    , lives : Int
    , helpScreens : List Texture
    , mapTexture : Texture
    }


type Model
    = Init InitData
    | Running RunningData
    | BetweenWaves BetweenWavesData
    | MainMenu MainMenuData


type alias InitData =
    { assetPackBuilder : AssetPackBuilder
    , error : Maybe String
    , randomSeed : Maybe Random.Seed
    , soundPack : Sounds.SoundPackBuilder
    }


type Msg
    = Frame Float
    | KeyboardEventReceived KeyboardEvent
    | TextureLoaded TextureKey (Maybe Canvas.Texture.Texture)
    | NewRandomNumber Int
    | StartPlayingBg Time.Posix Audio.Source
    | StartPlayingSoundEffect Time.Posix Audio.Source
    | SoundLoaded AssetPack.SoundKey (Result Audio.LoadError Audio.Source)


playBgMusicCommand : Audio.Source -> Cmd Msg
playBgMusicCommand audioSource =
    Time.now
        |> Task.perform (\time -> StartPlayingBg time audioSource)


playSoundEffectCommand : Audio.Source -> Cmd Msg
playSoundEffectCommand source =
    Time.now
        |> Task.perform (\time -> StartPlayingSoundEffect time source)


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


main : Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }


audio : Audio.AudioData -> Model -> Audio.Audio
audio _ model =
    let
        soundPack : Maybe SoundPack
        soundPack =
            case model of
                Init _ ->
                    Nothing

                Running runningData ->
                    Just runningData.shared.soundPack

                BetweenWaves betweenWavesData ->
                    Just betweenWavesData.shared.soundPack

                MainMenu mainMenuData ->
                    Just mainMenuData.soundPack
    in
    case soundPack of
        Just sp ->
            Audio.group
                (Audio.scaleVolume 1 sp.playingBg :: sp.playingEffects)

        _ ->
            Audio.silence


subscriptions : Audio.AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown <| Json.Decode.map KeyboardEventReceived decodeKeyboardEvent
        ]


renderWidth =
    800


renderHeightMap =
    400


renderHeight =
    440


mapWidth =
    200


mapHeight =
    100


mapToRenderPoint : MapPoint -> ( Float, Float )
mapToRenderPoint =
    MapPoint.toRenderPoint (renderWidth / mapWidth) (renderHeightMap / mapHeight)


init : flags -> ( Model, Cmd Msg, Audio.AudioCmd Msg )
init _ =
    ( Init
        { assetPackBuilder = initialAssetPackBuilder
        , error = Nothing
        , randomSeed = Nothing
        , soundPack = Sounds.initialSoundPackBuilder
        }
    , Random.generate NewRandomNumber (Random.int 0 Random.maxInt)
    , Audio.cmdBatch
        (List.map mapAudioLoadMsg AssetPack.loadAllSounds)
    )


mapAudioLoadMsg : AssetPack.LoadAudio -> Audio.AudioCmd Msg
mapAudioLoadMsg (AssetPack.LoadAudio key url) =
    Audio.loadAudio (SoundLoaded key) url


update : Audio.AudioData -> Msg -> Model -> ( Model, Cmd Msg, Audio.AudioCmd Msg )
update _ msg model =
    updateNoAudio msg model
        |> tupleAddThird Audio.cmdNone


updateNoAudio : Msg -> Model -> ( Model, Cmd Msg )
updateNoAudio msg model =
    case model of
        MainMenu data ->
            ( updateMainMenu msg data, Cmd.none )

        Running data ->
            updateRunning msg data
                |> Tuple.mapSecond soundToMsgCommand

        BetweenWaves data ->
            ( updateBetweenWaves msg data, Cmd.none )

        Init initData ->
            case msg of
                TextureLoaded id (Just t) ->
                    textureLoadedDuringInit id t initData
                        |> maybeInitFinished

                TextureLoaded _ Nothing ->
                    ( Init { initData | error = Just "Failed to load texture " }, Cmd.none )

                NewRandomNumber number ->
                    { initData | randomSeed = Just (Random.initialSeed number) }
                        |> maybeInitFinished

                SoundLoaded soundKey maybeSoundSource ->
                    case maybeSoundSource of
                        Ok source ->
                            soundLoadedDuringInit soundKey source initData
                                |> maybeInitFinished

                        Err _ ->
                            ( Init { initData | error = Just "Failed to load sound " }, Cmd.none )

                Frame _ ->
                    ( model, Cmd.none )

                KeyboardEventReceived _ ->
                    ( model, Cmd.none )

                StartPlayingBg _ _ ->
                    ( model, Cmd.none )

                StartPlayingSoundEffect _ _ ->
                    ( model, Cmd.none )


soundToMsgCommand : PlaySoundCommand -> Cmd Msg
soundToMsgCommand playSoundCommand =
    Cmd.batch
        ((case playSoundCommand.changeBgMusic of
            Just bgMusicSource ->
                playBgMusicCommand bgMusicSource

            _ ->
                Cmd.none
         )
            :: List.map playSoundEffectCommand playSoundCommand.addEffects
        )


type MainMenuState
    = InitialState
    | LevelSelect
    | LevelFinished Bool


type alias MainMenuData =
    { money : Money
    , levels : LevelsWithProgress
    , menu : MainMenuState
    , typedData : TypedData
    , texturePack : AssetPack
    , entityGen : EntityGen
    , soundPack : Sounds.SoundPack
    , firstTime : Bool
    }


textureLoadedDuringInit : TextureKey -> Texture -> InitData -> InitData
textureLoadedDuringInit id t initData =
    { initData | assetPackBuilder = AssetPack.insertTexture id t initData.assetPackBuilder }


soundLoadedDuringInit : SoundKey -> Audio.Source -> InitData -> InitData
soundLoadedDuringInit id sound initData =
    { initData | assetPackBuilder = AssetPack.insertSound id sound initData.assetPackBuilder }


maybeInitFinished : InitData -> ( Model, Cmd Msg )
maybeInitFinished initData =
    let
        maybeAp =
            tryBuildAssetPack initData.assetPackBuilder
    in
    case ( maybeAp, initData.randomSeed ) of
        ( Just ap, Just randomSeed ) ->
            ( initialMainMenu ap randomSeed, playBgMusicCommand ap.mainMenuBgSound )

        _ ->
            ( Init initData, Cmd.none )


initialMainMenu : AssetPack -> Random.Seed -> Model
initialMainMenu tp randomSeed =
    MainMenu <|
        resetMainMenu tp randomSeed Sounds.emptySoundPack


resetMainMenu : AssetPack -> Random.Seed -> SoundPack -> MainMenuData
resetMainMenu tp randomSeed soundPack =
    { money = Money.noMoney
    , levels = initialLevels
    , menu = InitialState
    , typedData = TypedData.initial
    , texturePack = tp
    , entityGen = EntityGen.make randomSeed
    , soundPack = soundPack
    , firstTime = True
    }


startGameLevelOne : AssetPack -> Random.Seed -> SoundPack -> Model
startGameLevelOne tp randomSeed soundPack =
    startGameGivenLevel (resetMainMenu tp randomSeed soundPack)


finishWave : RunningData -> Model
finishWave rd =
    case rd.remainingWaves of
        [] ->
            MainMenu
                { money = rd.shared.money
                , levels = Levels.completeCurrent rd.shared.allLevels
                , menu = LevelFinished True
                , typedData = rd.shared.typedData
                , texturePack = rd.shared.assetPack
                , entityGen = rd.shared.entityGen
                , soundPack = rd.shared.soundPack
                , firstTime = False
                }

        head :: rest ->
            BetweenWaves
                { time = 0
                , nextWave = head
                , remainingWaves = rest
                , shared = rd.shared
                }


startWave : BetweenWavesData -> RunningData
startWave bwd =
    let
        srd =
            bwd.shared
    in
    { currentWave = bwd.nextWave
    , shared = { srd | helpScreens = List.drop 1 srd.helpScreens }
    , remainingWaves = bwd.remainingWaves
    , stopSpawning = False
    , time = 0
    , zombies = []
    , projectiles = []
    }


startLevel : Level -> SharedRunningData -> RunningData
startLevel level srd =
    case level.waves of
        firstWave :: restWaves ->
            { currentWave = firstWave
            , remainingWaves = restWaves
            , stopSpawning = False
            , time = 0
            , zombies = []
            , shared = srd
            , projectiles = []
            }

        [] ->
            -- This should be impossible. Let's represent levels with a nonempty list maybe
            { currentWave = Levels.errorWave
            , remainingWaves = level.waves
            , stopSpawning = False
            , time = 0
            , zombies = []
            , shared = srd
            , projectiles = []
            }


updateMainMenu : Msg -> MainMenuData -> Model
updateMainMenu msg data =
    case msg of
        KeyboardEventReceived e ->
            let
                newTypedData =
                    TypedData.updateData e data.typedData
            in
            case newTypedData.completedWord of
                Just word ->
                    updateMainMenuWord word { data | typedData = TypedData.clearCompletedWord newTypedData }

                Nothing ->
                    MainMenu { data | typedData = newTypedData }

        StartPlayingBg time source ->
            MainMenu { data | soundPack = Sounds.setBg time source data.soundPack }

        StartPlayingSoundEffect time source ->
            MainMenu { data | soundPack = Sounds.addEffect time source data.soundPack }

        _ ->
            MainMenu data


updateMainMenuWord : String -> MainMenuData -> Model
updateMainMenuWord word data =
    case ( data.menu, word ) of
        ( InitialState, "NewGame" ) ->
            startGameLevelOne data.texturePack data.entityGen.randomSeed data.soundPack

        ( InitialState, "SelectLevel" ) ->
            MainMenu { data | menu = LevelSelect }

        ( InitialState, "NextLevel" ) ->
            if currentLevelCompleted data then
                tryStartNextLevel data

            else
                MainMenu data

        ( InitialState, "Retry" ) ->
            if hasProgress data then
                startGameGivenLevel data

            else
                MainMenu data

        ( InitialState, _ ) ->
            MainMenu data

        ( LevelSelect, _ ) ->
            if word == "Back" then
                MainMenu { data | menu = InitialState }

            else
                tryStartLevel word data

        ( LevelFinished _, "MainMenu" ) ->
            MainMenu { data | menu = InitialState }

        ( LevelFinished _, "Retry" ) ->
            startGameGivenLevel data

        ( LevelFinished _, "NextLevel" ) ->
            if Levels.isNextUnlocked data.levels then
                tryStartNextLevel data

            else
                MainMenu data

        ( LevelFinished _, _ ) ->
            MainMenu data


tryStartNextLevel : MainMenuData -> Model
tryStartNextLevel data =
    let
        newLevels : Maybe LevelsWithProgress
        newLevels =
            Levels.nextLevel data.levels
    in
    case newLevels of
        Just nextLevel ->
            startGameGivenLevel { data | levels = nextLevel }

        Nothing ->
            MainMenu data


tryStartLevel : String -> MainMenuData -> Model
tryStartLevel levelName data =
    let
        newLevels =
            Levels.tryStartLevel levelName data.levels
    in
    case newLevels of
        Just newLevelHolder ->
            startGameGivenLevel { data | levels = newLevelHolder }

        Nothing ->
            MainMenu data


startGameGivenLevel : MainMenuData -> Model
startGameGivenLevel data =
    let
        level =
            Levels.getCurrentLevel data.levels
    in
    Running <|
        startLevel level
            { buildSm = Menu.MainInGame
            , mapTexture = AssetPack.getTexture data.texturePack level.textureKey
            , allLevels = data.levels
            , entityGen = data.entityGen
            , assetPack = data.texturePack
            , soundPack = data.soundPack
            , towers = []
            , typedData = TypedData.initial
            , freeBuildPoints = level.buildPoints
            , timeSkew = 1
            , globalDelayedEffects = []
            , money = data.money
            , lives = 10
            , helpScreens =
                if data.firstTime then
                    [ data.texturePack.helpScreen ]

                else
                    []
            }


updateRunning : Msg -> RunningData -> ( Model, PlaySoundCommand )
updateRunning msg model =
    case msg of
        Frame dt ->
            tickRunning dt model

        KeyboardEventReceived e ->
            ( Running { model | shared = updateTypedData e model.shared }, soundCommandEmpty )

        StartPlayingSoundEffect time source ->
            let
                srd =
                    model.shared
            in
            ( Running { model | shared = { srd | soundPack = Sounds.addEffect time source srd.soundPack } }
            , soundCommandEmpty
            )

        _ ->
            ( Running model, soundCommandEmpty )


updateTypedData : KeyboardEvent -> SharedRunningData -> SharedRunningData
updateTypedData event srd =
    { srd | typedData = TypedData.updateData event srd.typedData }


updateBetweenWaves : Msg -> BetweenWavesData -> Model
updateBetweenWaves msg model =
    case msg of
        Frame dt ->
            tickBetweenWaves dt model

        KeyboardEventReceived e ->
            BetweenWaves { model | shared = updateTypedData e model.shared }

        StartPlayingSoundEffect time source ->
            let
                srd =
                    model.shared
            in
            BetweenWaves { model | shared = { srd | soundPack = Sounds.addEffect time source srd.soundPack } }

        _ ->
            BetweenWaves model


tickBetweenWaves : Float -> BetweenWavesData -> Model
tickBetweenWaves dt m =
    let
        tickResult =
            { buildSm = m.shared.buildSm
            , towers = m.shared.towers
            , freeBuildPoints =
                m.shared.freeBuildPoints
            , globalEffects = []
            , globalDelayedEffects = []
            , money = m.shared.money
            , quit = False
            , goToNextWave = False
            }
                |> tickMenuSystem
                    m.shared.typedData
                    m.shared.assetPack
                |> tickGoToNextWave
                    m.shared.typedData
    in
    if tickResult.quit then
        levelFailed tickResult.money m.shared.assetPack m.shared.allLevels m.shared.entityGen m.shared.soundPack

    else if tickResult.goToNextWave then
        Running <| startWave m

    else
        let
            srd =
                m.shared
        in
        BetweenWaves
            { m
                | shared =
                    { srd
                        | typedData =
                            TypedData.clearCompletedWord srd.typedData
                        , towers = tickResult.towers
                        , buildSm = tickResult.buildSm
                        , freeBuildPoints = tickResult.freeBuildPoints
                    }
                , time = m.time + dt
            }
            |> applyGlobalEffects tickResult.globalEffects


tickGoToNextWave :
    TypedData
    -> { a | goToNextWave : Bool }
    -> { a | goToNextWave : Bool }
tickGoToNextWave typedData tickData =
    let
        goToNextWave : Bool
        goToNextWave =
            case typedData.completedWord of
                Nothing ->
                    False

                Just word ->
                    word == "NextWave"
    in
    { tickData
        | goToNextWave = goToNextWave
    }


tickRunning : Float -> RunningData -> ( Model, PlaySoundCommand )
tickRunning dt0 m =
    let
        dt =
            dt0 * m.shared.timeSkew
    in
    initialTickEvent m
        |> moveZombies dt
        |> updateAllZombieWaypoints
        |> killZombies m.shared.typedData
        |> finishedZombies
        |> updateZombieAnimations dt
        |> spawnNewZombies m.currentWave.spawner m.shared.assetPack m.time (m.time + dt)
        |> tickMenuSystem m.shared.typedData m.shared.assetPack
        |> tickDelayedEffects dt
        |> tickZombieDelayedEffects dt
        |> moveProjectiles dt
        |> hitProjectiles
        |> towersFire m.shared.assetPack dt
        |> applyTickEvent dt m


tickZombieDelayedEffects : Float -> TickEvent -> TickEvent
tickZombieDelayedEffects dt te =
    let
        zombies =
            te.zombies
                |> List.map (zombieCountDownEffects dt)
    in
    { te | zombies = zombies }


zombieCountDownEffects : Float -> Zombie -> Zombie
zombieCountDownEffects dt zombie =
    let
        ( expired, remaining ) =
            zombie.delayedEffects
                |> List.map (\(Zombie.DelayedEffect e) -> { e | delay = e.delay - dt })
                |> List.partition (.delay >> (>) 0)
                |> Tuple.mapFirst (List.map .effect)
                |> Tuple.mapSecond (List.map Zombie.DelayedEffect)
    in
    { zombie | delayedEffects = remaining }
        |> applyEffectsToZombie expired


applyEffectsToZombie : List (Zombie -> Zombie) -> Zombie -> Zombie
applyEffectsToZombie effects zombie =
    List.foldl (\e z -> e z) zombie effects


tickDelayedEffects : Float -> TickEvent -> TickEvent
tickDelayedEffects dt te =
    let
        newGlobal =
            te.globalDelayedEffects
                |> List.map (\e -> { e | delay = e.delay - dt })
    in
    { te | globalDelayedEffects = newGlobal }


hitProjectiles : TickEvent -> TickEvent
hitProjectiles te =
    let
        ( newProjectiles, newZombies, anyHits ) =
            te.projectiles
                |> List.foldl hitProjectile ( [], te.zombies, False )
    in
    { te
        | projectiles = newProjectiles
        , zombies = newZombies
        , soundEffects =
            if anyHits then
                AssetPack.FreezerHit :: te.soundEffects

            else
                te.soundEffects
    }


projectileHitRange : Float
projectileHitRange =
    1


hitProjectile : Projectile -> ( List Projectile, List Zombie, Bool ) -> ( List Projectile, List Zombie, Bool )
hitProjectile proj ( projectiles, zombies, anyHits ) =
    let
        hitZombie : Maybe Zombie
        hitZombie =
            findZombieById proj.target zombies
                |> maybeFilter (.pos >> MapPoint.isWithin projectileHitRange proj.pos)
                |> Maybe.map (Projectile.applyEffect proj.effect)
    in
    case hitZombie of
        Just z ->
            ( projectiles, replaceZombieInList z zombies, True )

        Nothing ->
            ( proj :: projectiles, zombies, anyHits )


replaceZombieInList : Zombie -> List Zombie -> List Zombie
replaceZombieInList zombie =
    List.foldl
        (\z acc ->
            if EntityGen.zombieIdEqual z.id zombie.id then
                zombie :: acc

            else
                z :: acc
        )
        []


maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter f a =
    case a of
        Just value ->
            if f value then
                Just value

            else
                Nothing

        Nothing ->
            Nothing


moveProjectiles : Float -> TickEvent -> TickEvent
moveProjectiles dt te =
    let
        movedProjectiles =
            List.filterMap (moveProjectile dt te.zombies) te.projectiles
    in
    { te | projectiles = movedProjectiles }


moveProjectile : Float -> List Zombie -> Projectile -> Maybe Projectile
moveProjectile dt zombies proj =
    let
        targetPos : Maybe MapPoint
        targetPos =
            findZombieById proj.target zombies
                |> Maybe.map .pos

        newPosAndAngle =
            targetPos
                |> Maybe.map (MapPoint.moveTowardsWithAngle proj.speed dt proj.pos)
    in
    case newPosAndAngle of
        Just ( newPos, newAngle ) ->
            Just
                { proj
                    | pos = newPos
                    , direction = newAngle
                }

        Nothing ->
            Nothing


findZombieById : ZombieId -> List Zombie -> Maybe Zombie
findZombieById zid zombies =
    zombies
        |> List.filter (.id >> EntityGen.zombieIdEqual zid)
        |> List.head


towersFire : AssetPack -> Float -> TickEvent -> TickEvent
towersFire tp dt te =
    List.foldl (towerFire tp dt) ( [], te ) te.towers
        |> tupleMapBothToOne
            (\newTowers newTickEvent ->
                { newTickEvent | towers = newTowers }
            )


towerFire : AssetPack -> Float -> Tower -> ( List Tower, TickEvent ) -> ( List Tower, TickEvent )
towerFire tp dt tower ( towerList, te ) =
    if tower.reloadTimer > 0 then
        ( { tower | reloadTimer = tower.reloadTimer - dt } :: towerList, te )

    else
        let
            target : Maybe ZombieId
            target =
                te.zombies
                    |> List.map (\z -> ( z.id, z.pos ))
                    -- List (zombie id, zombie pos)
                    |> List.map (Tuple.mapSecond (MapPoint.distance tower.pos))
                    -- List (zombie id, distance)
                    |> List.Extra.minimumBy Tuple.second
                    -- Maybe (zombie id, distance)
                    |> Maybe.andThen
                        (\( zid, dist ) ->
                            if dist < tower.range then
                                Just zid

                            else
                                Nothing
                        )
        in
        case target of
            Just zombieId ->
                ( { tower | reloadTimer = tower.reloadTimer + tower.reloadInterval } :: towerList
                , { te
                    | projectiles = spawnProjectileForTowerType tower.towerType tp tower.pos zombieId :: te.projectiles
                    , soundEffects = AssetPack.FreezerFire :: te.soundEffects
                  }
                )

            _ ->
                ( tower :: towerList, te )


spawnProjectileForTowerType : Menu.TowerType -> AssetPack -> MapPoint -> ZombieId -> Projectile
spawnProjectileForTowerType towerType =
    case towerType of
        Menu.FreezerTower ->
            Projectile.spawnProjectileWithEffect Projectile.freezeEffect

        Menu.SimplifierTower ->
            Projectile.spawnProjectileWithEffect Projectile.shorterWordEffect


tupleMapBothToOne : (a -> b -> c) -> ( a, b ) -> c
tupleMapBothToOne f ( a, b ) =
    f a b


type alias PlaySoundCommand =
    { changeBgMusic : Maybe Audio.Source
    , addEffects : List Audio.Source
    }


soundCommandEmpty =
    { changeBgMusic = Nothing, addEffects = [] }


applyTickEvent : Float -> RunningData -> TickEvent -> ( Model, PlaySoundCommand )
applyTickEvent dt m tickResult =
    let
        ( expiredDelayedEffects, remainingDelayedEffects ) =
            tickResult.globalDelayedEffects
                |> List.partition (.delay >> (>) 0)
                |> Tuple.mapFirst (List.map .effect)
    in
    if tickResult.quit || tickResult.lives <= 0 then
        -- Level lose
        ( levelFailed tickResult.money m.shared.assetPack m.shared.allLevels m.shared.entityGen m.shared.soundPack
        , { changeBgMusic = Nothing
          , addEffects = [ m.shared.assetPack.levelFailed ]
          }
        )

    else
        let
            srd =
                m.shared

            newRd =
                { m
                    | zombies = tickResult.zombies
                    , shared =
                        { srd
                            | typedData = TypedData.clearCompletedWord m.shared.typedData
                            , buildSm = tickResult.buildSm
                            , towers = tickResult.towers
                            , entityGen = tickResult.entityGen
                            , globalDelayedEffects = remainingDelayedEffects
                            , money = tickResult.money
                            , lives = tickResult.lives
                            , freeBuildPoints = tickResult.freeBuildPoints
                        }
                    , time = m.time + dt
                    , stopSpawning = tickResult.stopSpawning
                    , projectiles = tickResult.projectiles
                }
        in
        if tickResult.stopSpawning && List.isEmpty tickResult.zombies then
            -- Wave (or level) win
            ( finishWave newRd
                |> applyGlobalEffects tickResult.globalEffects
                |> applyGlobalEffects expiredDelayedEffects
            , { changeBgMusic = Nothing
              , addEffects = [ m.shared.assetPack.levelSuccess ]
              }
            )

        else
            -- Keep going
            ( Running newRd
                |> applyGlobalEffects tickResult.globalEffects
                |> applyGlobalEffects expiredDelayedEffects
            , { changeBgMusic = Nothing
              , addEffects = List.map (flip AssetPack.getSound m.shared.assetPack) tickResult.soundEffects
              }
            )


levelFailed : Money -> AssetPack -> LevelsWithProgress -> EntityGen -> SoundPack -> Model
levelFailed money tp levels eg soundPack =
    MainMenu
        { money = money
        , texturePack = tp
        , menu = LevelFinished False
        , typedData = TypedData.initial
        , levels = levels
        , entityGen = eg
        , soundPack = soundPack
        , firstTime = False
        }


tickMenuSystem :
    TypedData
    -> AssetPack
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower.Tower
            , freeBuildPoints : List ( String, MapPoint )
            , globalEffects : List GlobalEffect
            , globalDelayedEffects : List GlobalEffectDelayed
            , money : Money
            , quit : Bool
        }
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , freeBuildPoints : List ( String, MapPoint )
            , globalEffects : List GlobalEffect
            , globalDelayedEffects : List GlobalEffectDelayed
            , money : Money
            , quit : Bool
        }
tickMenuSystem typedData tp te =
    case typedData.completedWord of
        Just word ->
            let
                maybeAction : Maybe ( Menu.MenuAction, Money, List ( String, MapPoint ) )
                maybeAction =
                    findMenuAction word te.buildSm te.money te.freeBuildPoints te.towers
            in
            case maybeAction of
                Just ( action, money, remainingBuildPoints ) ->
                    let
                        newTe =
                            buildSmActOn tp action te
                    in
                    { newTe
                        | money = money
                        , freeBuildPoints = remainingBuildPoints
                    }

                Nothing ->
                    te

        _ ->
            te


findMenuAction :
    String
    -> Menu.Selection
    -> Money
    -> List ( String, MapPoint )
    -> List Tower
    -> Maybe ( Menu.MenuAction, Money, List ( String, MapPoint ) )
findMenuAction word selection money freeBuildPoints towers =
    case selection of
        Menu.BuildTowerSelected towerType ->
            case
                findBuildPoint word freeBuildPoints
            of
                Just buildPoint ->
                    Just ( Menu.BuildTower towerType buildPoint, money, List.Extra.remove buildPoint freeBuildPoints )

                _ ->
                    case word of
                        "Cancel" ->
                            Just ( Menu.GotoMenu Menu.MainInGame, money, freeBuildPoints )

                        _ ->
                            Nothing

        Menu.Upgrade ->
            case findTowerAtBuildPoint word towers of
                Just bp ->
                    Just ( Menu.GotoMenu (Menu.UpgradeChoose bp), money, freeBuildPoints )

                _ ->
                    case word of
                        "Cancel" ->
                            Just ( Menu.GotoMenu Menu.MainInGame, money, freeBuildPoints )

                        _ ->
                            Nothing

        _ ->
            Menu.makeSelection word money selection
                |> Maybe.map (tupleAddThird freeBuildPoints)


findBuildPoint : String -> List ( String, MapPoint ) -> Maybe ( String, MapPoint )
findBuildPoint label buildPoints =
    buildPoints
        |> List.Extra.find (Tuple.first >> (==) label)


findTowerAtBuildPoint : String -> List Tower -> Maybe String
findTowerAtBuildPoint label towers =
    towers
        |> List.map .selectorLabel
        |> List.Extra.find ((==) label)


tupleAddThird : c -> ( a, b ) -> ( a, b, c )
tupleAddThird c ( a, b ) =
    ( a, b, c )


buildSmActOn :
    AssetPack
    -> Menu.MenuAction
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , freeBuildPoints : List ( String, MapPoint )
            , globalEffects : List GlobalEffect
            , globalDelayedEffects : List GlobalEffectDelayed
            , money : Money
            , quit : Bool
        }
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , freeBuildPoints : List ( String, MapPoint )
            , globalEffects : List GlobalEffect
            , globalDelayedEffects : List GlobalEffectDelayed
            , money : Money
            , quit : Bool
        }
buildSmActOn tp action te =
    case action of
        Menu.BuildTower towerType buildPoint ->
            menuBuildTowerSelected tp towerType buildPoint te

        Menu.UpgradeTower label upgradeType ->
            menuUpgradeTower label upgradeType te

        Menu.CastSpell _ ->
            { te
                | globalEffects = globalSlowmo 0.5 :: te.globalEffects
                , globalDelayedEffects = { effect = globalSlowmo 2, delay = 3000 } :: te.globalDelayedEffects
                , buildSm = Menu.MainInGame
            }

        Menu.GotoMenu menuSelection ->
            { te | buildSm = menuSelection }

        Menu.QuitLevel ->
            { te | quit = True }


menuUpgradeTower :
    String
    -> Menu.UpgradeType
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , money : Money
        }
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , money : Money
        }
menuUpgradeTower label upgradeType te =
    let
        upgradeFn =
            case upgradeType of
                Menu.SpeedUpgrade ->
                    speedUpgrade 1.4

                Menu.RangeUpgrade ->
                    rangeUpgrade 20
    in
    { te
        | towers = upgradeTowerWithLabel label upgradeFn te.towers
        , buildSm = Menu.MainInGame
    }


menuBuildTowerSelected :
    AssetPack
    -> TowerType
    -> ( String, MapPoint )
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , freeBuildPoints : List ( String, MapPoint )
            , money : Money
        }
    ->
        { a
            | buildSm : Menu.Selection
            , towers : List Tower
            , freeBuildPoints : List ( String, MapPoint )
            , money : Money
        }
menuBuildTowerSelected tp towerType buildPoint te =
    { te
        | buildSm = Menu.MainInGame
        , towers = Tower.buildTower tp towerType buildPoint :: te.towers
    }


type alias GlobalEffect =
    Model -> Model


type alias GlobalEffectDelayed =
    { effect : GlobalEffect
    , delay : Float
    }


applyGlobalEffects : List GlobalEffect -> Model -> Model
applyGlobalEffects effects model =
    List.foldl (\effect m -> effect m) model effects


globalSlowmo : Float -> GlobalEffect
globalSlowmo amount model =
    case model of
        Running rd ->
            let
                srd =
                    rd.shared
            in
            Running { rd | shared = { srd | timeSkew = srd.timeSkew * amount } }

        BetweenWaves rd ->
            let
                srd =
                    rd.shared
            in
            BetweenWaves { rd | shared = { srd | timeSkew = amount } }

        _ ->
            model


upgradeTowerWithLabel : String -> UpgradeFn -> List Tower -> List Tower
upgradeTowerWithLabel label f =
    listMapIf (.selectorLabel >> (==) label)
        f


type alias UpgradeFn =
    Tower -> Tower


speedUpgrade : Float -> UpgradeFn
speedUpgrade amount tower =
    { tower | reloadInterval = tower.reloadInterval / amount }


rangeUpgrade : Float -> UpgradeFn
rangeUpgrade amount tower =
    { tower | range = tower.range + amount }


listMapIf : (a -> Bool) -> (a -> a) -> List a -> List a
listMapIf predicate f =
    List.map
        (\a ->
            if predicate a then
                f a

            else
                a
        )


type alias TickEvent =
    { zombies : List Zombie
    , killedZombies : List Zombie
    , lives : Int
    , entityGen : EntityGen
    , stopSpawning : Bool
    , buildSm : Menu.Selection
    , towers : List Tower
    , projectiles : List Projectile
    , freeBuildPoints : List ( String, MapPoint )
    , globalEffects : List GlobalEffect
    , globalDelayedEffects : List GlobalEffectDelayed
    , money : Money
    , soundEffects : List AssetPack.SoundKey
    , quit : Bool
    }


initialTickEvent : RunningData -> TickEvent
initialTickEvent m =
    { zombies = m.zombies
    , killedZombies = []
    , lives = m.shared.lives
    , entityGen = m.shared.entityGen
    , stopSpawning = False
    , buildSm = m.shared.buildSm
    , towers = m.shared.towers
    , projectiles = m.projectiles
    , freeBuildPoints = m.shared.freeBuildPoints
    , globalEffects = []
    , globalDelayedEffects = m.shared.globalDelayedEffects
    , money = m.shared.money
    , soundEffects = []
    , quit = False
    }


spawnNewZombies : Levels.SpawnZombieFunction -> AssetPack -> Float -> Float -> TickEvent -> TickEvent
spawnNewZombies spawner tp timePrev timeNow te =
    case spawner te.entityGen tp timePrev timeNow of
        Just ( newZombies, newEg ) ->
            { te
                | zombies = te.zombies ++ newZombies
                , entityGen = newEg
            }

        Nothing ->
            { te | stopSpawning = True }


updateZombieAnimations : Float -> TickEvent -> TickEvent
updateZombieAnimations dt te =
    { te
        | zombies = List.map (updateAnimation dt) te.zombies
    }


updateAnimation : Float -> Zombie -> Zombie
updateAnimation dt z =
    { z | animationState = updateTrollAnim dt z.animationState }


updateAllZombieWaypoints : TickEvent -> TickEvent
updateAllZombieWaypoints te =
    { te | zombies = List.map updateZombieWaypoint te.zombies }


updateZombieWaypoint : Zombie -> Zombie
updateZombieWaypoint z =
    { z
        | wayPoints =
            case z.wayPoints of
                [] ->
                    []

                head :: rest ->
                    if MapPoint.isWithin 5 head z.pos then
                        rest

                    else
                        z.wayPoints
    }


moveZombies : Float -> TickEvent -> TickEvent
moveZombies dt te =
    { te
        | zombies =
            List.map
                (\z ->
                    let
                        target : MapPoint
                        target =
                            List.head z.wayPoints |> Maybe.withDefault z.pos
                    in
                    { z
                        | pos =
                            MapPoint.moveTowards z.speed dt z.pos target
                        , direction =
                            if MapPoint.rightOf z.pos target then
                                MapPoint.Right

                            else
                                MapPoint.Left
                    }
                )
                te.zombies
    }


shouldZombieDie : Maybe String -> Zombie -> Bool
shouldZombieDie maybeTypedWord z =
    case maybeTypedWord of
        Just typedWord ->
            z.word == typedWord && not (Zombie.hasEffect Zombie.Shielded z)

        Nothing ->
            False


killZombies : TypedData -> TickEvent -> TickEvent
killZombies typedData te =
    let
        ( killed, alive ) =
            List.partition
                (shouldZombieDie typedData.completedWord)
                te.zombies

        bounty =
            Money.Reward (List.length killed)
    in
    { te
        | zombies = alive
        , killedZombies = killed
        , money = Money.reward bounty te.money
        , soundEffects =
            if List.isEmpty killed then
                te.soundEffects

            else
                AssetPack.ZombieDie :: te.soundEffects
    }


finishedZombies : TickEvent -> TickEvent
finishedZombies te =
    let
        ( finished, remaining ) =
            List.partition (\z -> z.wayPoints == []) te.zombies

        livesLost =
            List.length finished
    in
    { te
        | lives = te.lives - livesLost
        , zombies = remaining
    }


view : Audio.AudioData -> Model -> Html Msg
view _ model =
    Element.layout [] <|
        case model of
            Init initData ->
                viewInit initData.error

            MainMenu data ->
                viewGame renderMainMenu viewDebugBarMainMenu data.typedData.typedSoFar data

            BetweenWaves m ->
                viewGame renderBetweenWaves viewDebugBarBetweenWaves m.shared.typedData.typedSoFar m

            Running m ->
                viewGame renderMainGame viewDebugBar m.shared.typedData.typedSoFar m


gameRenderXOffset =
    20


gameRenderYOffset =
    20


menuOffset =
    45


levelSelectXOffset =
    220


levelSelectYOffset =
    45


viewMainMenuLevelSelect : MainMenuData -> List LiveText
viewMainMenuLevelSelect data =
    (List.Extra.zip
        (List.range 0 5
            |> List.map (\i -> ( toFloat (224 + (i // 3) * levelSelectXOffset), toFloat (219 + levelSelectYOffset * remainderBy 3 i) ))
        )
        (Levels.asList data.levels)
        -- List (point, levelInfo)
        |> List.map (tupleMapBothToOne viewLevelSelectLevelInfo)
    )
        ++ [ { renderPoint = ( toFloat (224 + levelSelectXOffset), toFloat (219 + levelSelectYOffset * 2) )
             , text = "Back"
             , style = Alive LiveTextStyleMenu True
             }
           ]


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


viewLevelSelectLevelInfo : ( Float, Float ) -> Levels.LevelInfo -> LiveText
viewLevelSelectLevelInfo point levelInfo =
    { renderPoint = point
    , text = levelInfo.name ++ completionToEmoji levelInfo.completionStatus
    , style = Alive LiveTextStyleMenu (levelInfo.completionStatus /= Levels.Locked)
    }


completionToEmoji : Levels.CompletionStatus -> String
completionToEmoji s =
    case s of
        Levels.Locked ->
            "🔒"

        Levels.Completed ->
            "⭐"

        Levels.Unlocked ->
            "🔓"


viewMainMenuIntroTexts : MainMenuData -> List LiveText
viewMainMenuIntroTexts m =
    [ mainMenuIntroTextAtPos 0 "NewGame"
    , mainMenuIntroTextAtPos 1 "SelectLevel"
    ]
        |> listMaybeAppend
            (if currentLevelCompleted m then
                Just <| mainMenuIntroTextAtPos 2 "NextLevel"

             else if hasProgress m then
                Just <| mainMenuIntroTextAtPos 2 "Retry"

             else
                Nothing
            )


mainMenuIntroTextAtPos : Int -> String -> LiveText
mainMenuIntroTextAtPos pos text =
    { renderPoint = ( 345, 244 + toFloat (menuOffset * pos) ), text = text, style = Alive LiveTextStyleMenu True }


listMaybeAppend : Maybe a -> List a -> List a
listMaybeAppend maybe list =
    case maybe of
        Just a ->
            list ++ [ a ]

        Nothing ->
            list


levelFinishedXOffset =
    180


viewMainMenuLevelFinishedTexts : MainMenuData -> List LiveText
viewMainMenuLevelFinishedTexts m =
    [ { renderPoint = ( 157, 336 ), text = "MainMenu", style = Alive LiveTextStyleMenu True }
    , { renderPoint = ( 157 + levelFinishedXOffset, 336 ), text = "Retry", style = Alive LiveTextStyleMenu True }
    , { renderPoint = ( 157 + levelFinishedXOffset * 2, 336 )
      , text = "NextLevel"
      , style = Alive LiveTextStyleMenu (Levels.isNextUnlocked m.levels)
      }
    ]


currentLevelCompleted : MainMenuData -> Bool
currentLevelCompleted data =
    data.levels.current.completionStatus == Levels.Completed


hasProgress : MainMenuData -> Bool
hasProgress data =
    -- TODO What if you unlocked more than level one, but no completions and no money?
    data.money /= Money.noMoney || hasAnyCompletedLevel data.levels


hasAnyCompletedLevel : LevelsWithProgress -> Bool
hasAnyCompletedLevel =
    Levels.any (.completionStatus >> (==) Levels.Completed)


viewActiveTextAtPos : String -> LiveText -> Html msg
viewActiveTextAtPos typed { renderPoint, text, style } =
    case style of
        Alive styling enabled ->
            renderAliveText typed text styling enabled renderPoint

        Dead styling ->
            renderDeadText text styling renderPoint


renderAliveText : String -> String -> LiveTextStyle -> Bool -> ( Float, Float ) -> Html msg
renderAliveText typed text style enabled ( x, y ) =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            (case style of
                LiveTextStyleNextWaveHud ->
                    [ Css.position Css.absolute
                    , Css.left (Css.px (gameRenderXOffset + x))
                    , Css.top (Css.px (gameRenderYOffset + y))
                    , Css.transform (Css.translate (Css.pct -50))
                    , Css.fontSize (Css.px 50)
                    , Css.color (Css.hex "CCCCCC")
                    , Css.property "-webkit-text-stroke-width" "2.0px"
                    , Css.property "-webkit-text-stroke-color" "black"
                    ]

                _ ->
                    [ Css.position Css.absolute
                    , Css.left (Css.px (gameRenderXOffset + x))
                    , Css.top (Css.px (gameRenderYOffset + y))
                    ]
                        ++ (case style of
                                LiveTextStyleZombie _ ->
                                    [ Css.backgroundColor (Css.rgba 0 0 0 0.4)
                                    , Css.borderRadius (Css.px 10)
                                    , Css.padding2 (Css.px 1) (Css.px 6)
                                    , Css.transform (Css.translate (Css.pct -50))
                                    ]

                                _ ->
                                    []
                           )
            )
        ]
        (if enabled && firstCharEqual typed text then
            showEnabledLiveText style typed text

         else
            showDisabledLiveText style text
        )
        |> Html.Styled.toUnstyled


renderDeadText : String -> xDeadTextStyle -> ( Float, Float ) -> Html msg
renderDeadText text style ( x, y ) =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.position Css.absolute
            , Css.left (Css.px (gameRenderXOffset + x))
            , Css.top (Css.px (gameRenderYOffset + y))
            , Css.transform (Css.translate (Css.pct -50))
            , Css.fontSize (Css.px 50)
            , Css.color (Css.hex "CCCCCC")
            , Css.property "-webkit-text-stroke-width" "2.0px"
            , Css.property "-webkit-text-stroke-color" "black"
            ]
        ]
        [ Html.Styled.text text ]
        |> Html.Styled.toUnstyled


showEnabledLiveText : LiveTextStyle -> String -> String -> List (Html.Styled.Html msg)
showEnabledLiveText style typed text =
    Util.splitter typed text
        |> obfuscateMatchString style
        |> styleSplitWithMatch style


obfuscateMatchString : LiveTextStyle -> { matched : String, unmatched : String, left : String } -> { matched : String, unmatched : String, left : String }
obfuscateMatchString style matchStatus =
    case style of
        LiveTextStyleZombie (Blanks interval) ->
            Util.blankEveryMatchStatus interval matchStatus

        _ ->
            matchStatus


obfuscateText : LiveTextStyle -> String -> String
obfuscateText style word =
    case style of
        LiveTextStyleZombie (Blanks interval) ->
            Util.blankEvery interval word

        _ ->
            word


firstCharEqual : String -> String -> Bool
firstCharEqual w1 w2 =
    case ( w1 |> String.toList |> List.head, w2 |> String.toList |> List.head ) of
        ( Just a, Just b ) ->
            a == b

        _ ->
            True


styleSplitWithMatch : LiveTextStyle -> { matched : String, unmatched : String, left : String } -> List (Html.Styled.Html msg)
styleSplitWithMatch style { matched, unmatched, left } =
    [ Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.float Css.left
            , cssStyleLiveText Match style
            ]
        ]
        [ Html.Styled.text matched ]
    , Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.float Css.left
            , cssStyleLiveText Missmatch style
            ]
        ]
        [ Html.Styled.text unmatched ]
    , Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.float Css.left
            , cssStyleLiveText Leftover style
            ]
        ]
        [ Html.Styled.text left ]
    ]


type MatchStatus
    = Match
    | Missmatch
    | Leftover
    | Disabled


cssStyleLiveText : MatchStatus -> LiveTextStyle -> Css.Style
cssStyleLiveText ms style =
    Css.batch
        [ Css.color
            (case ms of
                Match ->
                    Css.hex "00ab00"

                Missmatch ->
                    Css.hex "ab0000"

                Leftover ->
                    Css.hex "ffffff"

                Disabled ->
                    Css.hex "b7b7b7"
            )
        , Css.fontFamilies [ "Georgia" ]
        , Css.property "-webkit-text-stroke-width" "0.5px"
        , Css.property "-webkit-text-stroke-color" "black"
        ]


showDisabledLiveText : LiveTextStyle -> String -> List (Html.Styled.Html msg)
showDisabledLiveText style text =
    [ Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.float Css.left
            , Css.color (Css.hex "676767")
            , cssStyleLiveText Disabled style
            ]
        ]
        [ Html.Styled.text (text |> obfuscateText style) ]
    ]


viewInit : Maybe String -> Element Msg
viewInit maybeErr =
    Element.column
        [ Element.centerX ]
        [ Element.html
            (Canvas.toHtmlWith
                { width = 0
                , height = 0
                , textures =
                    AssetPack.loadAllTextures
                        |> List.map (\(AssetPack.LoadTexture url key) -> Canvas.Texture.loadFromImageUrl url (TextureLoaded key))
                }
                []
                []
            )
        , Element.text
            (case maybeErr of
                Just err ->
                    "Error: " ++ err

                _ ->
                    "Loading assets ..."
            )
        ]


type LiveTextStyleZombieVariant
    = Basic
    | Blanks Int


type LiveTextStyle
    = LiveTextStyleZombie LiveTextStyleZombieVariant
    | LiveTextStyleMenu
    | LiveTextStyleBuildMenu
    | LiveTextStyleTower
    | LiveTextStyleNextWaveHud


type DeadTextStyle
    = DeadTextStyleNextWave


type DeadOrAlive
    = Alive LiveTextStyle Bool
    | Dead DeadTextStyle


type alias LiveText =
    { renderPoint : ( Float, Float )
    , text : String
    , style : DeadOrAlive
    }


viewGame : (a -> ( List Renderable, List LiveText )) -> (a -> Element Msg) -> String -> a -> Element Msg
viewGame fRender fDebug typed m =
    let
        ( renderables, liveTexts ) =
            fRender m
    in
    Element.column
        [ Element.spacing 20, Element.padding 20, Element.centerX ]
        [ Element.html
            (Canvas.toHtml
                ( renderWidth
                , renderHeight
                )
                [ style "border" "5px solid rgba(1.0,0,0,0.1)"
                ]
                renderables
            )

        --, fDebug m
        , Element.html (renderLiveTexts typed liveTexts)
        ]


renderLiveTexts : String -> List LiveText -> Html msg
renderLiveTexts typed liveTexts =
    Html.div []
        (List.map (renderLiveText typed) liveTexts)


renderLiveText : String -> LiveText -> Html msg
renderLiveText typed text =
    viewActiveTextAtPos typed text


renderBetweenWaves : BetweenWavesData -> ( List Renderable, List LiveText )
renderBetweenWaves model =
    ( [], [] )
        |> appendRenderable (renderBackground model.shared.mapTexture)
        |> appendRenderablesAndLiveText (renderUi model.shared)
        |> appendRenderablesAndLiveText (renderBuildChoices model.shared)
        |> appendRenderables (renderTowers model.shared.towers)
        |> appendMaybeRenderable (renderHelpScreen model.shared.helpScreens)
        |> appendLiveTexts (renderBetweenWavesHud model.time)


renderBetweenWavesHud : Float -> List LiveText
renderBetweenWavesHud timeToNextWave =
    [ { renderPoint = ( renderWidth / 2, renderHeightMap / 5 )
      , style = Alive LiveTextStyleNextWaveHud True
      , text = "NextWave"
      }
    ]


renderMainMenu : MainMenuData -> ( List Renderable, List LiveText )
renderMainMenu model =
    (case model.menu of
        InitialState ->
            ( [ renderBackground model.texturePack.mainMenu ]
            , viewMainMenuIntroTexts model
            )

        LevelSelect ->
            ( [ renderBackground model.texturePack.selectLevel ]
            , viewMainMenuLevelSelect model
            )

        LevelFinished success ->
            ( [ renderBackground model.texturePack.levelFinished
              , Canvas.text
                    [ Canvas.Settings.fill Color.darkOrange
                    , font
                        { size = 20
                        , family = "sans-serif"
                        }
                    ]
                    ( 175, 205 )
                    (if success then
                        "Level Complete! 🎉"

                     else
                        "Level Failed 😿"
                    )
              ]
            , viewMainMenuLevelFinishedTexts model
            )
    )
        |> appendRenderable (renderInfoBox model.texturePack.infoBox)
        |> appendRenderable (renderInfoBoxTextMoney model.money)


delayBetweenWaves =
    20000


timeToNextWaveStr : Float -> String
timeToNextWaveStr currentTime =
    String.fromInt <| round <| (delayBetweenWaves - currentTime) / 1000


renderMainGame : RunningData -> ( List Renderable, List LiveText )
renderMainGame model =
    ( [], [] )
        |> appendRenderable (renderBackground model.shared.mapTexture)
        |> appendRenderablesAndLiveText (renderUi model.shared)
        |> appendRenderablesAndLiveText (renderZombies model)
        |> appendRenderablesAndLiveText (renderBuildChoices model.shared)
        |> appendRenderables (renderTowers model.shared.towers)
        |> appendRenderables (List.map renderProjectile model.projectiles)


appendRenderable : Renderable -> ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText )
appendRenderable renderable ( renderables, liveTexts ) =
    ( renderables ++ [ renderable ], liveTexts )


appendLiveTexts : List LiveText -> ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText )
appendLiveTexts texts ( renderables, liveTexts ) =
    ( renderables, liveTexts ++ texts )


appendRenderables : List Renderable -> ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText )
appendRenderables newRenderables ( renderables, liveTexts ) =
    ( renderables ++ newRenderables, liveTexts )


appendRenderablesAndLiveText : ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText )
appendRenderablesAndLiveText ( newRenderables, newLiveTexts ) ( renderables, liveTexts ) =
    ( renderables ++ newRenderables, liveTexts ++ newLiveTexts )


appendMaybeRenderable : Maybe Renderable -> ( List Renderable, List LiveText ) -> ( List Renderable, List LiveText )
appendMaybeRenderable maybe l =
    case maybe of
        Just renderable ->
            appendRenderable renderable l

        Nothing ->
            l


renderUi : SharedRunningData -> ( List Renderable, List LiveText )
renderUi srd =
    ( [ Canvas.texture [] ( 0, 0 ) srd.assetPack.ui
      , renderInfoBoxText srd
      ]
    , renderBuildMenuText srd
    )


infoBoxTextX =
    675


infoBoxTextY =
    335


infoBoxTextOffset =
    20


renderInfoBoxText : SharedRunningData -> Renderable
renderInfoBoxText srd =
    Canvas.group []
        [ renderInfoBoxTextMoney srd.money
        , Canvas.text [ Canvas.Settings.fill Color.darkOrange ] ( infoBoxTextX, infoBoxTextY + infoBoxTextOffset ) ("Lives left: " ++ String.fromInt srd.lives)
        , Canvas.text [ Canvas.Settings.fill Color.darkOrange ] ( infoBoxTextX, infoBoxTextY + infoBoxTextOffset * 2 ) srd.allLevels.current.name
        , Canvas.text [ Canvas.Settings.fill Color.darkOrange ] ( infoBoxTextX, infoBoxTextY + infoBoxTextOffset * 3 ) (waveProgressText srd)
        ]


renderInfoBoxTextMoney : Money -> Renderable
renderInfoBoxTextMoney money =
    Canvas.text [ Canvas.Settings.fill Color.yellow ] ( infoBoxTextX, infoBoxTextY ) (Money.toString money)


waveProgressText : SharedRunningData -> String
waveProgressText srd =
    -- TODO Wave progress
    ""


{-| You are either building or upgrading a tower, and we want to highlight the available positions
-}
renderBuildChoices : SharedRunningData -> ( List Renderable, List LiveText )
renderBuildChoices srd =
    case srd.buildSm of
        Menu.BuildTowerSelected towerType ->
            -- Show the shadow of towers in every free tower position
            renderTowerPlacements srd.assetPack towerType srd.freeBuildPoints

        Menu.Upgrade ->
            -- Show the buildpoint text under eatch occupied tower
            List.map renderTowerPickText srd.towers
                |> List.unzip

        _ ->
            ( [], [] )


renderTowerPickText : Tower -> ( Renderable, LiveText )
renderTowerPickText tower =
    betterRenderTower (Alpha 0) tower.texture (Just tower.selectorLabel) tower.pos


renderTowerPlacements : AssetPack -> TowerType -> List ( String, MapPoint ) -> ( List Renderable, List LiveText )
renderTowerPlacements tp towerType freeBuildPoints =
    freeBuildPoints
        |> List.map (renderTowerPlacement tp towerType)
        |> List.unzip


renderTowerPlacement : AssetPack -> TowerType -> ( String, MapPoint ) -> ( Renderable, LiveText )
renderTowerPlacement tp towerType ( label, point ) =
    betterRenderTower (Alpha 0.5) (Tower.selectTowerTexture tp towerType) (Just label) point


type Alpha
    = Alpha Float


betterRenderTower : Alpha -> Texture -> Maybe String -> MapPoint -> ( Renderable, LiveText )
betterRenderTower (Alpha alpha) texture maybeLabel mapPoint =
    let
        point =
            mapToRenderPoint mapPoint

        { width, height } =
            Canvas.Texture.dimensions texture

        translateTexture =
            Canvas.Settings.Advanced.transform
                [ Canvas.Settings.Advanced.translate -(width / 2) -height
                ]
    in
    ( Canvas.texture [ translateTexture, Canvas.Settings.Advanced.alpha alpha ] point texture
    , { renderPoint = point, text = Maybe.withDefault "Humbug" maybeLabel, style = Alive LiveTextStyleTower True }
    )


renderProjectile : Projectile -> Canvas.Renderable
renderProjectile proj =
    let
        ( x, y ) =
            mapToRenderPoint proj.pos

        trans =
            Canvas.Settings.Advanced.translate

        rot =
            Canvas.Settings.Advanced.rotate

        transformSetting =
            Canvas.Settings.Advanced.transform
                [ trans x y
                , rot proj.direction
                , trans -15 -5
                ]
    in
    Canvas.texture [ transformSetting ] ( 0, 0 ) proj.texture


renderTowers : List Tower -> List Renderable
renderTowers towers =
    List.map renderTower towers


renderTower : Tower -> Renderable
renderTower t =
    betterRenderTower (Alpha 1) t.texture Nothing t.pos
        |> Tuple.first


renderLiveHud : String -> LiveText
renderLiveHud text =
    { renderPoint = ( renderWidth / 2, renderHeightMap / 5 )
    , style = Alive LiveTextStyleBuildMenu True
    , text = text
    }


renderBackground : Texture -> Canvas.Renderable
renderBackground t =
    Canvas.texture [] ( 0, 0 ) t


renderHelpScreen : List Texture -> Maybe Canvas.Renderable
renderHelpScreen hs =
    case List.head hs of
        Just helpTexture ->
            Just <| Canvas.texture [] ( 0, 0 ) helpTexture

        Nothing ->
            Nothing


renderInfoBox : Texture -> Canvas.Renderable
renderInfoBox t =
    Canvas.texture [] ( 650, 305 ) t


renderBuildMenuText : SharedRunningData -> List LiveText
renderBuildMenuText srd =
    Menu.edges srd.buildSm
        |> List.indexedMap (renderBuildMenuItem srd.money)


renderBuildMenuItem : Money -> Int -> Menu.Edge -> LiveText
renderBuildMenuItem money i { transition, cost } =
    { renderPoint = ( toFloat (43 + 190 * i), 410 )
    , style = Alive LiveTextStyleBuildMenu (Money.canAfford money cost)
    , text =
        transition
            ++ (if cost == Money.free then
                    ""

                else
                    " " ++ Money.costToString cost
               )
    }


renderZombies : RunningData -> ( List Renderable, List LiveText )
renderZombies m =
    List.map (renderZombie m.shared.assetPack) m.zombies
        |> List.unzip
        |> Tuple.mapSecond (List.filterMap identity)


renderZombie : AssetPack -> Zombie -> ( Renderable, Maybe LiveText )
renderZombie assetPack z =
    let
        point : ( Float, Float )
        point =
            mapToRenderPoint z.pos

        shieldPoint : ( Float, Float )
        shieldPoint =
            ( x - 35, y )

        ( x, y ) =
            point

        trollTexture : Texture
        trollTexture =
            renderTroll z.animationState

        isShielded : Bool
        isShielded =
            Zombie.hasEffect Zombie.Shielded z

        sprite : Canvas.Renderable
        sprite =
            Canvas.group []
                ([ Canvas.texture
                    [ Canvas.Settings.Advanced.transform
                        ([ Canvas.Settings.Advanced.translate -(toFloat z.animationState.spriteWidth / 2) (toFloat -z.animationState.spriteHeight)
                         , Canvas.Settings.Advanced.translate x y
                         ]
                            ++ orEmptyList (z.direction == MapPoint.Left)
                                [ Canvas.Settings.Advanced.scale -1 1
                                , Canvas.Settings.Advanced.translate -(toFloat z.animationState.spriteWidth * 3 / 4) 0
                                ]
                        )
                    ]
                    ( 0, 0 )
                    trollTexture
                 ]
                    |> listAppendIf isShielded
                        (Canvas.texture [] shieldPoint assetPack.textShield)
                )

        textStyle : LiveTextStyle
        textStyle =
            case Zombie.getBlanksEffectInterval z of
                Just v ->
                    LiveTextStyleZombie (Blanks v)

                _ ->
                    LiveTextStyleZombie Basic
    in
    ( sprite
    , if isShielded then
        Nothing

      else
        Just { renderPoint = point, text = z.word, style = Alive textStyle True }
    )


listAppendIf : Bool -> a -> List a -> List a
listAppendIf pred item list =
    if pred then
        item :: list

    else
        list


orEmptyList : Bool -> List a -> List a
orEmptyList pred list =
    if pred then
        list

    else
        []


viewDebugBar : RunningData -> Element Msg
viewDebugBar m =
    Element.column [ Element.spacing 10, Element.padding 10 ] <|
        viewSharedDebugBar m.shared
            ++ [ Element.text ("# Projectiles: " ++ String.fromInt (List.length m.projectiles)) ]
            ++ [ Element.text ("Remaining waves: " ++ String.fromInt (List.length m.remainingWaves))
               ]
            ++ List.map (\z -> Element.text (zombieString z)) m.zombies


viewDebugBarBetweenWaves : BetweenWavesData -> Element Msg
viewDebugBarBetweenWaves m =
    Element.column [ Element.spacing 10, Element.padding 10 ] <|
        viewSharedDebugBar m.shared


viewDebugBarMainMenu : MainMenuData -> Element Msg
viewDebugBarMainMenu m =
    Element.column [ Element.spacing 10, Element.padding 10 ] <|
        [ Element.text (Money.toString m.money) ]


viewSharedDebugBar : SharedRunningData -> List (Element Msg)
viewSharedDebugBar m =
    [ debugTypingInfoRow m.typedData
    , Element.text (Money.toString m.money)
    , debugTowers m.towers
    ]


debugTowers : List Tower -> Element Msg
debugTowers towers =
    Element.row [] <| List.map debugTower towers


debugTower : Tower -> Element Msg
debugTower t =
    Element.text ("T [rel= " ++ String.fromFloat t.reloadTimer ++ "] | ")


debugTypingInfoRow : TypedData -> Element msg
debugTypingInfoRow typedData =
    Element.row
        [ Element.spacing 20, Element.padding 20 ]
        [ Element.text ("typing: " ++ typedData.typedSoFar ++ ", ")
        , Element.text ("typed: " ++ viewTypedList typedData.completedWord typedData.completedWordQueue)
        , Element.text ("last: " ++ typedData.lastChar)
        ]


zombieString : Zombie -> String
zombieString z =
    "Z: [" ++ Zombie.toString z ++ "] " ++ MapPoint.toString z.pos


viewTypedList : Maybe String -> List String -> String
viewTypedList firstWord l =
    Maybe.withDefault "" firstWord ++ String.join ", " l
