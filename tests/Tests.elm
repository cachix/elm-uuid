module Tests exposing (all, buildUuid, initialSeedFuzzer, randomInt, uuidFuzzer)

--import Check exposing (claim, claimTrue, that, is, true, false, for, quickCheck)
--import Check.Test exposing (evidenceToTest)

import Expect
import Fuzz
import Uuid exposing (..)
import Random
import Random.Pcg.Extended as RandomE
import String
import Test exposing (..)


randomInt : Random.Generator Int
randomInt =
    Random.int RandomE.minInt RandomE.maxInt


buildUuid : Int -> Uuid
buildUuid integer =
    let
        initialSeed =
            RandomE.initialSeed integer []

        ( uuid, seed ) =
            RandomE.step generator initialSeed
    in
    uuid


initialSeedFuzzer : Fuzz.Fuzzer RandomE.Seed
initialSeedFuzzer =
    Fuzz.map (\x -> RandomE.initialSeed x []) (Fuzz.int)


uuidFuzzer : Fuzz.Fuzzer Uuid
uuidFuzzer =
    Fuzz.map buildUuid Fuzz.int


all : Test
all =
    describe "All tests"
        [ test "isValid - for valid uuid" <|
            \() ->
                "63B9AAA2-6AAF-473E-B37E-22EB66E66B76"
                    |> isValidUuid
                    |> Expect.equal True
                    |> Expect.onFail "should be valid"
        , test "isValid - for valid UUIDv7" <|
            \() ->
                "01890a5d-ac91-7590-bcdf-a4b23dff0e96"
                    |> isValidUuid
                    |> Expect.equal True
                    |> Expect.onFail "UUIDv7 should be valid"
        , test "isValid - for invalid uuid" <|
            \() ->
                "zz"
                    |> isValidUuid
                    |> Expect.equal False
                    |> Expect.onFail "should be invalid"
        , fuzz initialSeedFuzzer "generate uuid" <|
            \initialSeed ->
                let
                    ( uuid, nextSeed ) =
                        RandomE.step generator initialSeed
                in
                uuid
                    |> Uuid.toString
                    |> isValidUuid
                    |> Expect.equal True
                    |> Expect.onFail "should be valid uuid"
        , fuzz initialSeedFuzzer "generate two uuids" <|
            \initialSeed ->
                let
                    ( uuid1, seed1 ) =
                        RandomE.step generator initialSeed

                    ( uuid2, seed2 ) =
                        RandomE.step generator seed1
                in
                Expect.notEqual uuid1 uuid2
        , fuzz uuidFuzzer "roundtripping uuid through toString -> fromString keeps the Uuids intact" <|
            \uuid ->
                uuid
                    |> Uuid.toString
                    |> Uuid.fromString
                    |> Expect.equal (Just uuid)
        , fuzz uuidFuzzer "roundtripping uuid through toString -> fromString keeps the Uuids intact - upper casing is ignored" <|
            \uuid ->
                uuid
                    |> Uuid.toString
                    |> String.toUpper
                    |> Uuid.fromString
                    |> Expect.equal (Just uuid)
        , fuzz uuidFuzzer "roundtripping uuid through toString -> fromString keeps the Uuids intact - lower casing is ignored" <|
            \uuid ->
                uuid
                    |> Uuid.toString
                    |> String.toLower
                    |> Uuid.fromString
                    |> Expect.equal (Just uuid)
        ]
