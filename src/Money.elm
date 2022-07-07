module Money exposing (Cost(..), Money, Reward(..), buy, canAfford, costToString, free, noMoney, reward, toString)


type Money
    = Money Int


type Reward
    = Reward Int


type Cost
    = Cost Int


noMoney : Money
noMoney =
    Money 0


canAfford : Money -> Cost -> Bool
canAfford (Money m) (Cost c) =
    c <= m


buy : Cost -> Money -> Maybe Money
buy (Cost c) (Money m) =
    if m >= c then
        Just <| Money (m - c)

    else
        Nothing


toString : Money -> String
toString (Money m) =
    "$" ++ String.fromInt m


costToString : Cost -> String
costToString (Cost c) =
    "$" ++ String.fromInt c


reward : Reward -> Money -> Money
reward (Reward r) (Money m) =
    Money (m + r)


free : Cost
free =
    Cost 0
