module Menu exposing (Edge, LevelKey(..), MenuAction(..), Selection(..), SpellType(..), TowerType(..), UpgradeType(..), edges, makeSelection)

import List.Extra
import MapPoint exposing (MapPoint)
import Money exposing (Cost(..), Money)


type TowerType
    = FreezerTower
    | SimplifierTower


type Selection
    = MainInGame -- Show Build/Upgrade/Cast/Exit
    | Build -- Show list of towers
    | BuildTowerSelected TowerType -- Highlight selected tower
    | Upgrade -- Highlight upgrade button, while tower is being selected
    | UpgradeChoose String -- Show Speed/Range
    | Cast -- Show SlowMo/Thunderstorm


type alias Edge =
    { transition : String
    , cost : Money.Cost
    , action : MenuAction
    }


makeSelection : String -> Money -> Selection -> Maybe ( MenuAction, Money )
makeSelection typed money sel =
    let
        action : Maybe ( MenuAction, Cost )
        action =
            edges sel
                |> List.Extra.find (.transition >> (==) typed)
                |> Maybe.map (\i -> ( i.action, i.cost ))
    in
    case action of
        Just ( a, cost ) ->
            case Money.buy cost money of
                Just remainingMoney ->
                    Just ( a, remainingMoney )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


type SpellType
    = SlowMo


type LevelKey
    = Level2


type MenuAction
    = GotoMenu Selection
    | BuildTower TowerType ( String, MapPoint )
    | UpgradeTower String UpgradeType
    | CastSpell SpellType
    | QuitLevel


edges : Selection -> List Edge
edges s =
    case s of
        MainInGame ->
            [ Edge "Build" Money.free (GotoMenu Build)
            , Edge "Cast" Money.free (GotoMenu Cast)
            , Edge "Upgrade" Money.free (GotoMenu Upgrade)
            , Edge "Exit" Money.free QuitLevel
            ]

        Build ->
            [ Edge "Freezer" (costOfTower FreezerTower) (GotoMenu (BuildTowerSelected FreezerTower))
            , Edge "Simplifier" (costOfTower SimplifierTower) (GotoMenu (BuildTowerSelected SimplifierTower))
            , Edge "Cancel" Money.free (GotoMenu MainInGame)
            ]

        UpgradeChoose label ->
            [ Edge "Speed" (costOfUpgrade SpeedUpgrade) (UpgradeTower label SpeedUpgrade)
            , Edge "Range" (costOfUpgrade RangeUpgrade) (UpgradeTower label RangeUpgrade)
            , Edge "Cancel" Money.free (GotoMenu MainInGame)
            ]

        Cast ->
            [ Edge "Slowmo" (Money.Cost 10) (CastSpell SlowMo)
            , Edge "Cancel" Money.free (GotoMenu MainInGame)
            ]

        Upgrade ->
            [ Edge "Cancel" Money.free (GotoMenu MainInGame)
            ]

        BuildTowerSelected _ ->
            [ Edge "Cancel" Money.free (GotoMenu MainInGame)
            ]


costOfTower : TowerType -> Cost
costOfTower tt =
    case tt of
        FreezerTower ->
            Cost 40

        SimplifierTower ->
            Cost 60


type UpgradeType
    = SpeedUpgrade
    | RangeUpgrade


costOfUpgrade : UpgradeType -> Cost
costOfUpgrade upgradeType =
    case upgradeType of
        SpeedUpgrade ->
            Cost 50

        RangeUpgrade ->
            Cost 30
