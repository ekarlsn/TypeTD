module Projectile exposing (Effect, Projectile, applyEffect, freezeEffect, shorterWordEffect, spawnProjectileWithEffect)

import Canvas.Texture exposing (Texture)
import EntityGen exposing (ZombieId)
import Gen.AssetPack exposing (AssetPack)
import MapPoint exposing (MapPoint)
import WordList
import Zombie exposing (Zombie)


type alias Projectile =
    { speed : Float
    , pos : MapPoint
    , direction : Float -- Radians
    , target : ZombieId
    , effect : Effect
    , texture : Texture
    }


projectileSpeed : Float
projectileSpeed =
    60


spawnProjectileWithEffect : Effect -> AssetPack -> MapPoint -> ZombieId -> Projectile
spawnProjectileWithEffect effect tp pos zid =
    { speed = projectileSpeed
    , pos = pos
    , direction = 0
    , target = zid
    , effect = effect
    , texture = tp.arrow
    }


freezeFunc : Float -> Zombie -> Zombie
freezeFunc amount z =
    { z | speed = z.speed * amount }


freezeEffect : Effect
freezeEffect =
    TimedEffect (freezeFunc 0.5) (freezeFunc 2) 800


shorterWordEffect : Effect
shorterWordEffect =
    OneShot shorterWordFunc


shorterWordFunc : Zombie -> Zombie
shorterWordFunc z =
    { z | word = WordList.findShorterWord z.word }


type Effect
    = OneShot (Zombie -> Zombie)
    | TimedEffect (Zombie -> Zombie) (Zombie -> Zombie) Float


applyEffect : Effect -> Zombie -> Zombie
applyEffect effect z =
    case effect of
        OneShot f ->
            f z

        TimedEffect applyF revertF timer ->
            applyF z
                |> (\zb ->
                        { zb
                            | delayedEffects =
                                Zombie.DelayedEffect
                                    { delay = timer
                                    , effect = revertF
                                    }
                                    :: zb.delayedEffects
                        }
                   )
