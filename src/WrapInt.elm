module WrapInt exposing (WrapInt(..), create, eq, inc)


type WrapInt
    = WrapInt Int Int


inc : WrapInt -> WrapInt
inc (WrapInt max v) =
    WrapInt max (modBy max (v + 1))


create : { max : Int, value : Int } -> WrapInt
create { max, value } =
    WrapInt max (modBy max value)


eq : Int -> WrapInt -> Bool
eq i (WrapInt max v) =
    i == v
