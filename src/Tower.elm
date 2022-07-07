module Tower exposing (Tower, buildTower, selectTowerTexture)

import Canvas.Texture exposing (Texture)
import Gen.AssetPack exposing (AssetPack)
import MapPoint exposing (MapPoint)
import Menu exposing (TowerType(..))


type alias Tower =
    { towerType : Menu.TowerType
    , pos : MapPoint
    , selectorLabel : String
    , texture : Canvas.Texture.Texture
    , reloadTimer : Float
    , reloadInterval : Float
    , range : Float
    }


buildTower : AssetPack -> TowerType -> ( String, MapPoint ) -> Tower
buildTower tp towerType ( label, pos ) =
    case towerType of
        FreezerTower ->
            { towerType = towerType
            , pos = pos
            , selectorLabel = label
            , texture = selectTowerTexture tp towerType
            , reloadTimer = 2000
            , reloadInterval = 2000
            , range = 70
            }

        SimplifierTower ->
            { towerType = towerType
            , pos = pos
            , selectorLabel = label
            , texture = selectTowerTexture tp towerType
            , reloadTimer = 2000
            , reloadInterval = 7000
            , range = 35
            }


selectTowerTexture : AssetPack -> TowerType -> Texture
selectTowerTexture tp towerType =
    case towerType of
        SimplifierTower ->
            tp.towerSimplifier

        FreezerTower ->
            tp.towerFreezer
