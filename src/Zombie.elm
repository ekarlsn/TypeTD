module Zombie exposing
    ( AnimationState
    , DelayedEffect(..)
    , Effect(..)
    , Zombie
    , ZombieTemplate(..)
    , ZombieTemplateDone
    , applyEffect
    , basicZombieTemplate
    , getBlanksEffectInterval
    , hasEffect
    , makeZombieGen
    , pendingApplyEffect
    , pendingRemoveEffect
    , removeEffect
    , renderTroll
    , toString
    , updateTrollAnim
    , withDelayedEffects
    , withEffects
    , withSpeed
    , withTexture
    , withWayPoints
    , withWords
    )

import Canvas.Texture exposing (Texture)
import EntityGen exposing (ZombieId)
import Gen.AssetPack as AssetPack exposing (AssetPack)
import List.Extra
import MapPoint exposing (MapPoint, RightLeft)
import WordList exposing (WordList)


type alias Zombie =
    { id : ZombieId
    , pos : MapPoint
    , word : String
    , wayPoints : List MapPoint
    , speed : Float
    , animationState : AnimationState
    , delayedEffects : List DelayedEffect
    , direction : RightLeft
    , activeEffects : List Effect
    }


type DelayedEffect
    = DelayedEffect
        { delay : Float
        , effect : Zombie -> Zombie
        }


applyEffect : Effect -> Zombie -> Zombie
applyEffect effect z =
    { z | activeEffects = effect :: z.activeEffects }


removeEffect : Effect -> Zombie -> Zombie
removeEffect effect z =
    { z | activeEffects = List.Extra.remove effect z.activeEffects }


pendingApplyEffect : Effect -> Float -> DelayedEffect
pendingApplyEffect effect delay =
    DelayedEffect
        { delay = delay
        , effect = applyEffect effect
        }


pendingRemoveEffect : Effect -> Float -> DelayedEffect
pendingRemoveEffect effect delay =
    DelayedEffect
        { delay = delay
        , effect = removeEffect effect
        }


type alias AnimationState =
    { now : Float
    , delay : Float
    , sheetRows : Int
    , sheetCols : Int
    , spriteWidth : Int
    , spriteHeight : Int
    , sheet : Texture
    }


type Effect
    = Shielded
    | Blanks Int


hasEffect : Effect -> Zombie -> Bool
hasEffect effect z =
    List.any ((==) effect) z.activeEffects


getBlanksEffectInterval : Zombie -> Maybe Int
getBlanksEffectInterval z =
    List.filterMap
        (\e ->
            case e of
                Blanks v ->
                    Just v

                _ ->
                    Nothing
        )
        z.activeEffects
        |> List.head


type ZombieTemplate a
    = ZombieTemplate
        { speed : Float
        , startPoint : MapPoint
        , wayPoints : List MapPoint
        , textureLoadingId : AssetPack.TextureKey
        , wordList : ( String, List String )
        , effects : List Effect
        , delayedEffects : List DelayedEffect
        }


type alias ZombieTemplateDone =
    ZombieTemplate {}


basicZombieTemplate : ZombieTemplate { needWaypoints : (), needTexture : (), needWordList : () }
basicZombieTemplate =
    ZombieTemplate
        { speed = 10
        , startPoint = MapPoint.MapPoint 0 0
        , wayPoints = []
        , textureLoadingId = AssetPack.Troll
        , wordList = ( "Hello", [ "Goodbye" ] )
        , effects = []
        , delayedEffects = []
        }


withTexture : AssetPack.TextureKey -> ZombieTemplate { a | needTexture : () } -> ZombieTemplate a
withTexture id (ZombieTemplate template) =
    ZombieTemplate { template | textureLoadingId = id }


withSpeed : Float -> ZombieTemplate a -> ZombieTemplate a
withSpeed speed (ZombieTemplate template) =
    ZombieTemplate { template | speed = speed }


withWords : WordList -> ZombieTemplate { a | needWordList : () } -> ZombieTemplate a
withWords words (ZombieTemplate template) =
    ZombieTemplate { template | wordList = words }


withEffects : List Effect -> ZombieTemplate a -> ZombieTemplate a
withEffects effects (ZombieTemplate template) =
    ZombieTemplate { template | effects = effects }


withDelayedEffects : List DelayedEffect -> ZombieTemplate a -> ZombieTemplate a
withDelayedEffects delayedEffects (ZombieTemplate template) =
    ZombieTemplate { template | delayedEffects = delayedEffects }


withWayPoints : ( MapPoint, List MapPoint ) -> ZombieTemplate { a | needWaypoints : () } -> ZombieTemplate a
withWayPoints ( startPoint, wayPoints ) (ZombieTemplate template) =
    ZombieTemplate
        { template
            | startPoint = startPoint
            , wayPoints = wayPoints
        }


makeZombieGen : AssetPack -> ZombieTemplateDone -> EntityGen.Gen Zombie
makeZombieGen tp (ZombieTemplate template) =
    let
        fuzzedMapPointGen : EntityGen.Gen (List MapPoint)
        fuzzedMapPointGen =
            template.wayPoints
                |> List.map (MapPoint.fuzzGen 8)
                |> EntityGen.sequence
    in
    EntityGen.map4
        (makeNew
            template.speed
            (AssetPack.getTexture tp template.textureLoadingId)
            template.effects
            template.delayedEffects
        )
        EntityGen.nextZombieIdGen
        (WordList.randomWordGen template.wordList)
        (MapPoint.fuzzGen 4 template.startPoint)
        fuzzedMapPointGen


makeNew :
    Float
    -> Texture
    -> List Effect
    -> List DelayedEffect
    -> ZombieId
    -> String
    -> MapPoint
    -> List MapPoint
    -> Zombie
makeNew speed texture effects delayedEffects id word startPoint waypoints =
    { id = id
    , pos = startPoint
    , word = word
    , wayPoints = waypoints
    , speed = speed
    , direction = MapPoint.Right
    , activeEffects = effects
    , delayedEffects = delayedEffects
    , animationState =
        { now = 0
        , sheet = texture
        , spriteHeight = 64
        , spriteWidth = 64
        , delay = 400 / speed
        , sheetRows = 4
        , sheetCols = 5
        }
    }


updateTrollAnim : Float -> AnimationState -> AnimationState
updateTrollAnim dt animState =
    { animState | now = animState.now + dt }


renderTroll : AnimationState -> Texture
renderTroll animState =
    let
        spriteCount =
            animState.sheetCols * animState.sheetRows

        spriteNum =
            remainderBy spriteCount (floor (animState.now / animState.delay))

        x =
            remainderBy animState.sheetCols spriteNum

        y =
            spriteNum // animState.sheetCols
    in
    Canvas.Texture.sprite
        { x = toFloat (x * animState.spriteWidth)
        , y = toFloat (y * animState.spriteHeight)
        , width = toFloat animState.spriteWidth
        , height = toFloat animState.spriteHeight
        }
        animState.sheet


toString : Zombie -> String
toString z =
    EntityGen.zombieIdStr z.id
