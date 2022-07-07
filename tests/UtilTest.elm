module UtilTest exposing (SandboxType(..), obfuscationTests, randomTests, sandbox, splitterTests)

import Expect exposing (Expectation)
import MapPoint exposing (MapPoint(..))
import Random
import Random.Extra
import Test exposing (..)
import Util exposing (..)
import WordList


type SandboxType
    = SandboxTwo Int


sandboxValue =
    SandboxTwo 1


sandbox : Test
sandbox =
    Test.describe "sandbox"
        [ test "one" <| \_ -> sandboxValue |> Expect.equal (SandboxTwo 1) ]


obfuscationTests : Test
obfuscationTests =
    Test.describe "Obfuscate"
        [ test "nothing written 2" <|
            \_ ->
                blankEveryMatchStatus 2 { matched = "", unmatched = "", left = "hello" }
                    |> Expect.equal { matched = "", unmatched = "", left = "h_l_o" }
        , test "nothing written 3" <|
            \_ ->
                blankEveryMatchStatus 3 { matched = "", unmatched = "", left = "hello" }
                    |> Expect.equal { matched = "", unmatched = "", left = "he_lo" }
        , test "nothing written 4" <|
            \_ ->
                blankEveryMatchStatus 4 { matched = "", unmatched = "", left = "hello" }
                    |> Expect.equal { matched = "", unmatched = "", left = "hel_o" }
        , test "some written 2" <|
            \_ ->
                blankEveryMatchStatus 2 { matched = "ab", unmatched = "", left = "cdefghi" }
                    |> Expect.equal { matched = "ab", unmatched = "", left = "c_e_g_i" }
        , test "one written" <|
            \_ ->
                blankEveryMatchStatus 2 { matched = "a", unmatched = "", left = "bcdefg" }
                    |> Expect.equal { matched = "a", unmatched = "", left = "_c_e_g" }
        , test "some unmatched" <|
            \_ ->
                blankEveryMatchStatus 2 { matched = "he", unmatched = "ll", left = "o" }
                    |> Expect.equal { matched = "he", unmatched = "l_", left = "o" }
        , test "blank unmatched 2" <|
            \_ ->
                blankEveryMatchStatus 2 { matched = "a", unmatched = "b", left = "cdef" }
                    |> Expect.equal { matched = "a", unmatched = "_", left = "c_e_" }
        , test "blank unmatched 3" <|
            \_ ->
                blankEveryMatchStatus 3 { matched = "ab", unmatched = "c", left = "def" }
                    |> Expect.equal { matched = "ab", unmatched = "_", left = "de_" }
        ]


splitterTests : Test
splitterTests =
    Test.describe "Splitter tests"
        [ test "split three ways" <|
            \_ -> splitter "exteraaa" "exterminate" |> Expect.equal { matched = "exter", unmatched = "min", left = "ate" }
        , test "split all matching ways" <|
            \_ -> splitter "exterminate" "exterminate" |> Expect.equal { matched = "exterminate", unmatched = "", left = "" }
        , test "match and left" <|
            \_ -> splitter "exter" "exterminate" |> Expect.equal { matched = "exter", unmatched = "", left = "minate" }
        , test "typed after end" <|
            \_ -> splitter "hio" "hi" |> Expect.equal { matched = "hi", unmatched = "o", left = "" }
        , test "missmatch and past end" <|
            \_ -> splitter "hao" "hi" |> Expect.equal { matched = "h", unmatched = "io", left = "" }
        ]


randomTests : Test
randomTests =
    Test.describe "Fuzz some stuff"
        [ test "Fuzz a point" <|
            \_ ->
                MapPoint.fuzz 3 (MapPoint 3 3)
                    |> testInvariant (MapPoint.isWithin 4.5 (MapPoint 3 3))
        , test "Get a word" <|
            \_ ->
                let
                    words =
                        ( "balls", [ "hello", "hi", "bye", "something", "random" ] )
                in
                WordList.getRandomWordGenerator words
                    |> testInvariant (\w -> List.member w (Tuple.first words :: Tuple.second words))
        ]


testInvariant : (a -> Bool) -> Random.Generator a -> Expectation
testInvariant pred gen =
    List.repeat 500 gen
        |> Random.Extra.sequence
        |> Util.flip Random.step (Random.initialSeed 42)
        |> Tuple.first
        |> List.all pred
        |> Expect.true "Invariant not satisfied"
