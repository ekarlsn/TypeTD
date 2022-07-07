module Util exposing (MatchStatus, blankEvery, blankEveryMatchStatus, flip, splitter)

import WrapInt exposing (WrapInt)


splitter : String -> String -> { matched : String, unmatched : String, left : String }
splitter typed word =
    List.foldl folderFunc { matched = [], unmatched = [], left = String.toList word } (String.toList typed)
        |> (\{ matched, unmatched, left } ->
                { matched = String.fromList (List.reverse matched)
                , unmatched = (List.reverse >> String.fromList) unmatched
                , left = String.fromList left
                }
           )


folderFunc : Char -> { matched : List Char, unmatched : List Char, left : List Char } -> { matched : List Char, unmatched : List Char, left : List Char }
folderFunc c { matched, unmatched, left } =
    case left of
        h :: tail ->
            if c == h && unmatched == [] then
                { matched = c :: matched, unmatched = [], left = tail }

            else
                { matched = matched, unmatched = h :: unmatched, left = tail }

        [] ->
            -- Past the end of the original word
            { matched = matched, unmatched = c :: unmatched, left = [] }


type alias MatchStatus =
    { matched : String, unmatched : String, left : String }


blankEvery : Int -> String -> String
blankEvery interval word =
    blankEveryWithOffset (WrapInt.create { max = interval, value = 1 }) word


blankEveryMatchStatus : Int -> MatchStatus -> MatchStatus
blankEveryMatchStatus interval ms =
    let
        leftOffset : WrapInt
        leftOffset =
            WrapInt.create { max = interval, value = String.length ms.matched + String.length ms.unmatched + 1 }

        unmatchedOffset : WrapInt
        unmatchedOffset =
            WrapInt.create { max = interval, value = String.length ms.matched + 1 }
    in
    { ms
        | unmatched = blankEveryWithOffset unmatchedOffset ms.unmatched
        , left = blankEveryWithOffset leftOffset ms.left
    }


blankEveryWithOffset : WrapInt -> String -> String
blankEveryWithOffset offset word =
    String.foldl
        (\c ( strBuild, toggler ) ->
            ( (if WrapInt.eq 0 toggler then
                '_'

               else
                c
              )
                :: strBuild
            , WrapInt.inc toggler
            )
        )
        ( [], offset )
        word
        |> Tuple.first
        |> List.reverse
        |> String.fromList


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
