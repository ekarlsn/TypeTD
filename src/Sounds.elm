module Sounds exposing (LoadAudio(..), SoundKey(..), SoundPack, SoundPackBuilder, SoundPackTemplate, addEffect, emptySoundPack, initialSoundPackBuilder, setBg)

import Audio
import Time


type alias SoundPackTemplate a =
    { mainMenuBg : a
    , levelFailed : a
    , levelSuccess : a
    , zombieDie : a
    , freezerHit : a
    , freezerFire : a
    }


type alias SoundPackBuilder =
    SoundPackTemplate (Maybe Audio.Source)


type alias SoundPack =
    { playingEffects : List Audio.Audio
    , playingBg : Audio.Audio
    }


emptySoundPack : SoundPack
emptySoundPack =
    { playingEffects = []
    , playingBg = Audio.silence
    }


type LoadAudio
    = LoadAudio SoundKey String


initialSoundPackBuilder : SoundPackBuilder
initialSoundPackBuilder =
    { mainMenuBg = Nothing
    , levelFailed = Nothing
    , levelSuccess = Nothing
    , zombieDie = Nothing
    , freezerHit = Nothing
    , freezerFire = Nothing
    }


type SoundKey
    = ZombieDie


setBg : Time.Posix -> Audio.Source -> SoundPack -> SoundPack
setBg time source soundPack =
    { soundPack | playingBg = Audio.audio source time }


addEffect : Time.Posix -> Audio.Source -> SoundPack -> SoundPack
addEffect time source soundPack =
    let
        newEffects : List Audio.Audio
        newEffects =
            soundPack.playingEffects
                |> (::) (Audio.audio source time)
                |> List.take 5
    in
    { soundPack | playingEffects = newEffects }
