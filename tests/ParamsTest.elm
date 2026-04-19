module ParamsTest exposing (suite)

import Expect
import Main exposing
    ( FloorCovering(..)
    , N50Method(..)
    , Orientation(..)
    , RoofType(..)
    , ShelterFactor(..)
    , ThermalMass(..)
    , VentMode(..)
    , decodeParams
    , defaultModel
    , encodeParams
    , paramsVersion
    )
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Params v1 unpacking"
        [ test "round-trips the default model" <|
            \_ ->
                encodeParams defaultModel
                    |> decodeParams
                    |> Expect.equal defaultModel
        , test "round-trips a modified model" <|
            \_ ->
                let
                    modified =
                        { defaultModel
                            | totalFloorArea = "450"
                            , wallU = "0.14"
                            , yFactor = "0.079"
                            , tempOut = "-5"
                        }
                in
                modified
                    |> encodeParams
                    |> decodeParams
                    |> Expect.equal modified
        , test "missing trailing fields fall back to defaults" <|
            \_ ->
                -- Simulate an older URL that has only the first few fields.
                -- Decoder should backfill the rest from defaultModel.
                let
                    partial =
                        encodeParams defaultModel |> List.take 5

                    decoded =
                        decodeParams partial
                in
                Expect.equal decoded.pvOrientation defaultModel.pvOrientation
        , test "empty list decodes to default model" <|
            \_ ->
                decodeParams []
                    |> Expect.equal defaultModel

        -- Wire-format pin: this exact float list represents a saved v1 URL.
        -- Every field has a distinct, non-default value so any field-order
        -- swap, enum reordering, or encoding change will fail this test —
        -- meaning existing user URLs would silently break.
        -- To intentionally break compatibility, bump paramsVersion and
        -- create a new pin test for v2.
        , test "v1 wire format: pinned payload decodes to known values" <|
            \_ ->
                let
                    pinned =
                        [ 275       -- totalFloorArea
                        , 3         -- numFloors
                        , 2.4       -- floorHeight
                        , 0         -- roofType: FlatRoof
                        , 40        -- pitchAngle
                        , 0.18      -- wallU
                        , 0.15      -- roofU
                        , 0.11      -- floorU
                        , 25        -- glazingPct
                        , 1.2       -- glazingU
                        , 0.05      -- yFactor
                        , 0         -- ventMode: DirectACH
                        , 0.6       -- ach
                        , 4.5       -- n50
                        , 2         -- n50Method: HEM
                        , 3         -- shelterFactor: Heavy
                        , 20        -- tempIn
                        , -5        -- tempOut
                        , 140       -- heatedFloorArea
                        , 1         -- floorCovering: Wood
                        , 40        -- flowTemp
                        , 7         -- emitterDeltaT
                        , 2400      -- hdd
                        , 5         -- pvKwp
                        , 1050      -- pvIrradiation
                        , 2         -- pvOrientation: EastWest
                        ]

                    expected =
                        { totalFloorArea = "275"
                        , numFloors = "3"
                        , floorHeight = "2.4"
                        , roofType = FlatRoof
                        , pitchAngle = "40"
                        , wallU = "0.18"
                        , roofU = "0.15"
                        , floorU = "0.11"
                        , glazingPct = "25"
                        , glazingU = "1.2"
                        , yFactor = "0.05"
                        , ventMode = DirectACH
                        , ach = "0.6"
                        , n50 = "4.5"
                        , n50Method = HEM
                        , shelterFactor = Heavy
                        , tempIn = "20"
                        , tempOut = "-5"
                        , heatedFloorArea = "140"
                        , floorCovering = Wood
                        , flowTemp = "40"
                        , emitterDeltaT = "7"
                        , hdd = "2400"
                        , pvKwp = "5"
                        , pvIrradiation = "1050"
                        , pvOrientation = EastWest

                        -- These fields were not present in the original v1
                        -- payload. The decoder backfills from defaults,
                        -- confirming forward-compat still holds for this URL.
                        , gValue = "0.6"
                        , thermalMass = MediumMass
                        , summerTempOut = "28"
                        , householdElecKwh = "3500"
                        , evMilesPerYear = "8000"
                        , batteryKwh = "5"
                        , annualDhwKwh = "2500"
                        , dayRate = "27"
                        , nightRate = "7"
                        , exportRate = "15"
                        }
                in
                decodeParams pinned
                    |> Expect.equal expected
        ]
