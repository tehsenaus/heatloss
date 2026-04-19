port module Main exposing
    ( FloorCovering(..)
    , Model
    , N50Method(..)
    , Orientation(..)
    , RoofType(..)
    , ShelterFactor(..)
    , ThermalMass(..)
    , VentMode(..)
    , decodeParams
    , defaultModel
    , encodeParams
    , main
    , paramsVersion
    )

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


port saveParams : List Float -> Cmd msg


main : Program (Maybe (List Float)) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type VentMode
    = DirectACH
    | FromN50


type N50Method
    = RuleOfThumb
    | SAP2012
    | HEM


type ShelterFactor
    = Exposed
    | Slight
    | Moderate
    | Heavy


type RoofType
    = FlatRoof
    | VaultedRoof


type FloorCovering
    = Tile
    | Wood
    | Carpet


floorCoeff : FloorCovering -> Float
floorCoeff c =
    case c of
        Tile   -> 6.7
        Wood   -> 5.0
        Carpet -> 3.7


floorLabel : FloorCovering -> String
floorLabel c =
    case c of
        Tile   -> "Tile / stone"
        Wood   -> "Engineered wood"
        Carpet -> "Carpet"


type Orientation
    = South
    | SouthEastWest
    | EastWest
    | North


type ThermalMass
    = LightMass
    | MediumMass
    | HeavyMass


-- Thermal time constant in hours (EN ISO 13790 typical values)
thermalMassTau : ThermalMass -> Float
thermalMassTau t =
    case t of
        LightMass  -> 50
        MediumMass -> 110
        HeavyMass  -> 240


thermalMassLabel : ThermalMass -> String
thermalMassLabel t =
    case t of
        LightMass  -> "Light (timber frame)"
        MediumMass -> "Medium (cavity masonry)"
        HeavyMass  -> "Heavy (solid masonry / concrete)"


-- ISO 13790 utilisation factor for solar/internal gains.
-- gamma = gain/loss ratio for the period; tau = thermal time constant (h).
utilisationFactor : Float -> Float -> Float
utilisationFactor gamma tau =
    let
        a = 1 + tau / 15
    in
    if gamma < 0 then
        1

    else if abs (gamma - 1) < 1.0e-6 then
        a / (a + 1)

    else
        (1 - gamma ^ a) / (1 - gamma ^ (a + 1))


orientationLabel : Orientation -> String
orientationLabel o =
    case o of
        South         -> "South"
        SouthEastWest -> "SE / SW"
        EastWest      -> "E / W"
        North         -> "North"


-- Tilt/orientation factor relative to horizontal. Linear interp on pitch.
tiltOrientFactor : Orientation -> Float -> Float
tiltOrientFactor orient pitch =
    let
        points =
            case orient of
                -- Tilt/orientation factors vs horizontal annual irradiation
                -- (UK ~52°N, calibrated against PVGIS)
                South         -> [ (0, 1.00), (20, 1.12), (35, 1.16), (50, 1.10), (70, 0.95), (90, 0.70) ]
                SouthEastWest -> [ (0, 1.00), (20, 1.04), (35, 1.05), (50, 0.98), (70, 0.82), (90, 0.60) ]
                EastWest      -> [ (0, 1.00), (20, 0.95), (35, 0.88), (50, 0.78), (70, 0.65), (90, 0.50) ]
                North         -> [ (0, 1.00), (20, 0.78), (35, 0.62), (50, 0.50), (70, 0.38), (90, 0.27) ]
    in
    interpolate points (clamp 0 90 pitch)


interpolate : List ( Float, Float ) -> Float -> Float
interpolate pts x =
    case pts of
        [] -> 0
        [ ( _, y ) ] -> y
        ( x1, y1 ) :: (( x2, y2 ) :: _ as rest) ->
            if x <= x2 then
                y1 + (y2 - y1) * (x - x1) / (x2 - x1)
            else
                interpolate rest x


performanceRatio : Float
performanceRatio =
    0.80


shelterMultiplier : ShelterFactor -> Float
shelterMultiplier s =
    case s of
        Exposed  -> 1.0
        Slight   -> 0.9
        Moderate -> 0.8
        Heavy    -> 0.7


shelterLabel : ShelterFactor -> String
shelterLabel s =
    case s of
        Exposed  -> "Exposed (×1.0)"
        Slight   -> "Slight — 1 side (×0.9)"
        Moderate -> "Moderate — 2 sides (×0.8)"
        Heavy    -> "Heavy — 3+ sides (×0.7)"


windFactor : N50Method -> Float
windFactor method =
    case method of
        RuleOfThumb -> 1 / 20
        SAP2012     -> 0.07
        HEM         -> 0.079



-- MODEL


type alias Model =
    { totalFloorArea : String
    , numFloors : String
    , floorHeight : String
    , roofType : RoofType
    , pitchAngle : String
    , wallU : String
    , roofU : String
    , floorU : String
    , glazingPct : String
    , glazingU : String
    , yFactor : String
    , ventMode : VentMode
    , ach : String
    , n50 : String
    , n50Method : N50Method
    , shelterFactor : ShelterFactor
    , tempIn : String
    , tempOut : String
    , heatedFloorArea : String
    , floorCovering : FloorCovering
    , flowTemp : String
    , emitterDeltaT : String
    , hdd : String
    , pvKwp : String
    , pvIrradiation : String
    , pvOrientation : Orientation
    , gValue : String
    , thermalMass : ThermalMass
    , summerTempOut : String
    , householdElecKwh : String
    , evMilesPerYear : String
    , batteryKwh : String
    , annualDhwKwh : String
    , dayRate : String
    , nightRate : String
    , exportRate : String
    , standingCharge : String
    }


paramsVersion : Float
paramsVersion =
    1


init : Maybe (List Float) -> ( Model, Cmd Msg )
init flag =
    let
        model =
            case flag of
                Just (v :: rest) ->
                    if v == paramsVersion then
                        decodeParams rest

                    else
                        defaultModel

                _ ->
                    defaultModel
    in
    ( model, Cmd.none )


defaultModel : Model
defaultModel =
    { totalFloorArea = "300"
    , numFloors = "2.5"
    , floorHeight = "2.5"
    , roofType = VaultedRoof
    , pitchAngle = "35"
    , wallU = "0.14"
    , roofU = "0.13"
    , floorU = "0.13"
    , glazingPct = "20"
    , glazingU = "1.4"
    , yFactor = "0.08"
    , ventMode = FromN50
    , ach = "0.5"
    , n50 = "3"
    , n50Method = SAP2012
    , shelterFactor = Slight
    , tempIn = "21"
    , tempOut = "-3"
    , heatedFloorArea = "120"
    , floorCovering = Tile
    , flowTemp = "35"
    , emitterDeltaT = "5"
    , hdd = "2200"
    , pvKwp = "4"
    , pvIrradiation = "990"
    , pvOrientation = South
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
    , standingCharge = "60"
    }



-- PARAMS ENCODE / DECODE


toF : String -> Float
toF s =
    String.toFloat s |> Maybe.withDefault 0


fromF : Float -> String
fromF f =
    String.fromFloat (toFloat (round (f * 10000)) / 10000)


roofTypeToF : RoofType -> Float
roofTypeToF r =
    case r of
        FlatRoof    -> 0
        VaultedRoof -> 1


roofTypeFromF : Float -> RoofType
roofTypeFromF f =
    if f < 0.5 then FlatRoof else VaultedRoof


ventModeToF : VentMode -> Float
ventModeToF v =
    case v of
        DirectACH -> 0
        FromN50   -> 1


ventModeFromF : Float -> VentMode
ventModeFromF f =
    if f < 0.5 then DirectACH else FromN50


n50MethodToF : N50Method -> Float
n50MethodToF m =
    case m of
        RuleOfThumb -> 0
        SAP2012     -> 1
        HEM         -> 2


n50MethodFromF : Float -> N50Method
n50MethodFromF f =
    case round f of
        0 -> RuleOfThumb
        1 -> SAP2012
        _ -> HEM


shelterFactorToF : ShelterFactor -> Float
shelterFactorToF s =
    case s of
        Exposed  -> 0
        Slight   -> 1
        Moderate -> 2
        Heavy    -> 3


shelterFactorFromF : Float -> ShelterFactor
shelterFactorFromF f =
    case round f of
        0 -> Exposed
        1 -> Slight
        2 -> Moderate
        _ -> Heavy


floorCoveringToF : FloorCovering -> Float
floorCoveringToF c =
    case c of
        Tile   -> 0
        Wood   -> 1
        Carpet -> 2


floorCoveringFromF : Float -> FloorCovering
floorCoveringFromF f =
    case round f of
        0 -> Tile
        1 -> Wood
        _ -> Carpet


orientationToF : Orientation -> Float
orientationToF o =
    case o of
        South         -> 0
        SouthEastWest -> 1
        EastWest      -> 2
        North         -> 3


orientationFromF : Float -> Orientation
orientationFromF f =
    case round f of
        0 -> South
        1 -> SouthEastWest
        2 -> EastWest
        _ -> North


thermalMassToF : ThermalMass -> Float
thermalMassToF t =
    case t of
        LightMass  -> 0
        MediumMass -> 1
        HeavyMass  -> 2


thermalMassFromF : Float -> ThermalMass
thermalMassFromF f =
    case round f of
        0 -> LightMass
        1 -> MediumMass
        _ -> HeavyMass


encodeParams : Model -> List Float
encodeParams m =
    [ toF m.totalFloorArea
    , toF m.numFloors
    , toF m.floorHeight
    , roofTypeToF m.roofType
    , toF m.pitchAngle
    , toF m.wallU
    , toF m.roofU
    , toF m.floorU
    , toF m.glazingPct
    , toF m.glazingU
    , toF m.yFactor
    , ventModeToF m.ventMode
    , toF m.ach
    , toF m.n50
    , n50MethodToF m.n50Method
    , shelterFactorToF m.shelterFactor
    , toF m.tempIn
    , toF m.tempOut
    , toF m.heatedFloorArea
    , floorCoveringToF m.floorCovering
    , toF m.flowTemp
    , toF m.emitterDeltaT
    , toF m.hdd
    , toF m.pvKwp
    , toF m.pvIrradiation
    , orientationToF m.pvOrientation
    , toF m.gValue
    , thermalMassToF m.thermalMass
    , toF m.summerTempOut
    , toF m.householdElecKwh
    , toF m.evMilesPerYear
    , toF m.batteryKwh
    , toF m.annualDhwKwh
    , toF m.dayRate
    , toF m.nightRate
    , toF m.exportRate
    , toF m.standingCharge
    ]


decodeParams : List Float -> Model
decodeParams floats =
    let
        arr = Array.fromList floats

        getF : Int -> Float -> Float
        getF i default =
            Array.get i arr |> Maybe.withDefault default

        getS : Int -> String -> String
        getS i default =
            Array.get i arr |> Maybe.map fromF |> Maybe.withDefault default

        d = defaultModel
    in
    { totalFloorArea  = getS 0  d.totalFloorArea
    , numFloors       = getS 1  d.numFloors
    , floorHeight     = getS 2  d.floorHeight
    , roofType        = roofTypeFromF (getF 3 (roofTypeToF d.roofType))
    , pitchAngle      = getS 4  d.pitchAngle
    , wallU           = getS 5  d.wallU
    , roofU           = getS 6  d.roofU
    , floorU          = getS 7  d.floorU
    , glazingPct      = getS 8  d.glazingPct
    , glazingU        = getS 9  d.glazingU
    , yFactor         = getS 10 d.yFactor
    , ventMode        = ventModeFromF (getF 11 (ventModeToF d.ventMode))
    , ach             = getS 12 d.ach
    , n50             = getS 13 d.n50
    , n50Method       = n50MethodFromF (getF 14 (n50MethodToF d.n50Method))
    , shelterFactor   = shelterFactorFromF (getF 15 (shelterFactorToF d.shelterFactor))
    , tempIn          = getS 16 d.tempIn
    , tempOut         = getS 17 d.tempOut
    , heatedFloorArea = getS 18 d.heatedFloorArea
    , floorCovering   = floorCoveringFromF (getF 19 (floorCoveringToF d.floorCovering))
    , flowTemp        = getS 20 d.flowTemp
    , emitterDeltaT   = getS 21 d.emitterDeltaT
    , hdd             = getS 22 d.hdd
    , pvKwp           = getS 23 d.pvKwp
    , pvIrradiation   = getS 24 d.pvIrradiation
    , pvOrientation   = orientationFromF (getF 25 (orientationToF d.pvOrientation))
    , gValue          = getS 26 d.gValue
    , thermalMass     = thermalMassFromF (getF 27 (thermalMassToF d.thermalMass))
    , summerTempOut   = getS 28 d.summerTempOut
    , householdElecKwh = getS 29 d.householdElecKwh
    , evMilesPerYear  = getS 30 d.evMilesPerYear
    , batteryKwh      = getS 31 d.batteryKwh
    , annualDhwKwh    = getS 32 d.annualDhwKwh
    , dayRate         = getS 33 d.dayRate
    , nightRate       = getS 34 d.nightRate
    , exportRate      = getS 35 d.exportRate
    , standingCharge  = getS 36 d.standingCharge
    }



-- UPDATE


type Msg
    = SetTotalFloorArea String
    | SetNumFloors String
    | SetFloorHeight String
    | SetRoofType RoofType
    | SetPitchAngle String
    | SetWallU String
    | SetRoofU String
    | SetFloorU String
    | SetGlazingPct String
    | SetGlazingU String
    | SetYFactor String
    | SetVentMode VentMode
    | SetACH String
    | SetN50 String
    | SetN50Method N50Method
    | SetShelterFactor ShelterFactor
    | SetTempIn String
    | SetTempOut String
    | SetHeatedFloorArea String
    | SetFloorCovering FloorCovering
    | SetFlowTemp String
    | SetEmitterDeltaT String
    | SetHDD String
    | SetPvKwp String
    | SetPvIrradiation String
    | SetPvOrientation Orientation
    | SetGValue String
    | SetThermalMass ThermalMass
    | SetSummerTempOut String
    | SetHouseholdElecKwh String
    | SetEvMilesPerYear String
    | SetBatteryKwh String
    | SetAnnualDhwKwh String
    | SetDayRate String
    | SetNightRate String
    | SetExportRate String
    | SetStandingCharge String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            updateField msg model
    in
    ( newModel, saveParams (paramsVersion :: encodeParams newModel) )


updateField : Msg -> Model -> Model
updateField msg model =
    case msg of
        SetTotalFloorArea v -> { model | totalFloorArea = v }
        SetNumFloors v      -> { model | numFloors = v }
        SetFloorHeight v    -> { model | floorHeight = v }
        SetRoofType t       -> { model | roofType = t }
        SetPitchAngle v     -> { model | pitchAngle = v }
        SetWallU v          -> { model | wallU = v }
        SetRoofU v          -> { model | roofU = v }
        SetFloorU v         -> { model | floorU = v }
        SetGlazingPct v     -> { model | glazingPct = v }
        SetGlazingU v       -> { model | glazingU = v }
        SetYFactor v        -> { model | yFactor = v }
        SetVentMode m       -> { model | ventMode = m }
        SetACH v            -> { model | ach = v }
        SetN50 v            -> { model | n50 = v }
        SetN50Method m      -> { model | n50Method = m }
        SetShelterFactor f  -> { model | shelterFactor = f }
        SetTempIn v         -> { model | tempIn = v }
        SetTempOut v        -> { model | tempOut = v }
        SetHeatedFloorArea v -> { model | heatedFloorArea = v }
        SetFloorCovering c  -> { model | floorCovering = c }
        SetFlowTemp v       -> { model | flowTemp = v }
        SetEmitterDeltaT v  -> { model | emitterDeltaT = v }
        SetHDD v            -> { model | hdd = v }
        SetPvKwp v          -> { model | pvKwp = v }
        SetPvIrradiation v  -> { model | pvIrradiation = v }
        SetPvOrientation o  -> { model | pvOrientation = o }
        SetGValue v         -> { model | gValue = v }
        SetThermalMass t    -> { model | thermalMass = t }
        SetSummerTempOut v  -> { model | summerTempOut = v }
        SetHouseholdElecKwh v -> { model | householdElecKwh = v }
        SetEvMilesPerYear v -> { model | evMilesPerYear = v }
        SetBatteryKwh v     -> { model | batteryKwh = v }
        SetAnnualDhwKwh v   -> { model | annualDhwKwh = v }
        SetDayRate v        -> { model | dayRate = v }
        SetNightRate v      -> { model | nightRate = v }
        SetExportRate v     -> { model | exportRate = v }
        SetStandingCharge v -> { model | standingCharge = v }



-- EFFECTIVE ACH


effectiveACH : Model -> Maybe Float
effectiveACH m =
    case m.ventMode of
        DirectACH ->
            String.toFloat m.ach

        FromN50 ->
            String.toFloat m.n50
                |> Maybe.map
                    (\n50 ->
                        case m.n50Method of
                            RuleOfThumb -> n50 / 20
                            SAP2012     -> n50 * windFactor SAP2012 * shelterMultiplier m.shelterFactor
                            HEM         -> n50 * windFactor HEM     * shelterMultiplier m.shelterFactor
                    )



-- COOLING CONSTANTS
--
-- Internal heat gains: occupants + lighting + appliances. EN ISO 13790
-- default for residential is ~3-5 W/m². We use 4 W/m² as a continuous
-- average — a simplification (real loads are spiky) but adequate for
-- monthly/design sizing.
internalGainsWperM2 : Float
internalGainsWperM2 =
    4


-- Indoor cooling setpoint (constant). Typical residential summer comfort
-- target. Kept separate from heating tempIn since they're conceptually
-- different (heating to 21°C, cooling above 25°C — a 4°C dead band).
coolingSetpoint : Float
coolingSetpoint =
    25


-- UK monthly average outdoor air temperature (°C). Used to estimate
-- "free cooling" capacity through the fabric in the monthly breakdown:
-- when avg Tout < 25°C the building can shed solar/internal gains
-- passively. Source: typical UK Met Office climate normals.
monthlyAvgTempC : List Float
monthlyAvgTempC =
    [ 4.5, 4.5, 6.5, 8.5, 11.5, 14.5, 16.5, 16.5, 14.0, 10.5, 6.5, 4.5 ]


-- Diurnal temperature swing (°C) — daytime is roughly this much above the
-- 24h average and nighttime this much below. UK summer typical ~3°C; we
-- use it year-round as a rough constant. Used so we don't credit
-- night-time fabric losses against daytime cooling demand (comfort needs
-- to be maintained through the day).
diurnalSwing : Float
diurnalSwing =
    3


-- Seasonal cooling COP (SEER-equivalent) for an ASHP run in reverse.
-- UK summer conditions: chilled flow ~16°C, outdoor ~20°C avg → Carnot
-- ~70, real-world field SEER for residential ASHPs sits ~3–4. We use 3.5
-- as a representative figure. Lower than heating SCOP would suggest from
-- Carnot alone because of part-load cycling and dehumidification overhead.
-- (Note: UFH-as-cooling has condensation/dewpoint constraints that may
-- limit usable flow temps; not modelled here.)
coolingScop : Float
coolingScop =
    3.5


-- Domestic hot water heating by ASHP. DHW needs ~50–55°C flow, well
-- above space-heating flow temps, so SCOP is lower. UK field data puts
-- DHW SCOP around 2.2–2.8 depending on cylinder/controls; we use 2.5.
dhwScop : Float
dhwScop =
    2.5


-- Home battery round-trip efficiency (AC-in to AC-out). Typical
-- Li-ion residential systems: 85–92%.
batteryEff : Float
batteryEff =
    0.9


-- EV charging efficiency (socket to drive-battery). AC charger + onboard
-- rectifier losses ~8–12%.
evChargeEff : Float
evChargeEff =
    0.9


-- Grid export limit (kW). UK G98 notification allows single-phase export up
-- to 3.68 kW without DNO approval; beyond this the inverter curtails.
-- Oversized arrays therefore lose summer generation at the midday peak.
exportLimitKw : Float
exportLimitKw =
    3.68


-- Monthly DHW demand shape (normalised, sums to 1.0). Slight winter
-- bias reflects colder incoming mains water (~5°C Jan vs ~15°C Jul)
-- which increases kWh-per-litre delivered at a fixed setpoint.
dhwMonthlyFractions : List Float
dhwMonthlyFractions =
    [ 0.094, 0.086, 0.090, 0.081, 0.078, 0.072, 0.072, 0.072, 0.076, 0.083, 0.086, 0.090 ]


-- EV efficiency (Wh/mile) by monthly avg outdoor temp. Cold weather hurts
-- range via battery, cabin heating, and tyre/aero losses; hot weather adds
-- A/C overhead but is milder. Mapped linearly between 350 (Jan, ~4.5°C)
-- and 250 (Jul, ~16.5°C) Wh/mile, clamped at the extremes.
evWhPerMile : Float -> Float
evWhPerMile avgTempC =
    Basics.clamp 250 350 (350 - (avgTempC - 4.5) * (100 / 12))


-- Internal gain split day/night. Occupants/cooking/lighting peak during
-- waking hours; some baseload (fridge, standby) overnight. Rough 60/40.
internalGainDayFrac : Float
internalGainDayFrac =
    0.6


-- Estimate peak instantaneous horizontal irradiance (W/m²) from annual
-- horizontal irradiation (kWh/m²/yr). Peak is bounded by clear-sky
-- atmospheric maximum (~950 W/m² at UK latitudes); annual is mostly
-- cloud-driven. In UK range (800–1100 kWh/yr) the linear scaling holds;
-- sunnier climates saturate. The 0.85 coefficient was chosen so the UK
-- default (990 kWh/yr) yields ~840 W/m².
peakHorizFromAnnual : Float -> Float
peakHorizFromAnnual annual =
    Basics.min 950 (0.85 * annual)


-- Internal blinds: typical residential occupants close blinds when sun
-- is direct, transmitting ~60% of incident solar. Applied to peak (design)
-- cooling only — the annual/monthly utilisation calc keeps full incident
-- since blinds aren't used continuously through the year.
blindsFactor : Float
blindsFactor =
    0.6



-- CALCULATION


toRad : Float -> Float
toRad deg =
    deg * pi / 180


type alias Results =
    { footprint : Float
    , opaqueWallArea : Float
    , glazingArea : Float
    , roofArea : Float
    , gableArea : Float
    , ridgeHeight : Float
    , totalExposedArea : Float
    , htb : Float
    , volume : Float
    , effectiveAch : Float
    , deltaT : Float
    , qWalls : Float
    , qGlazing : Float
    , qRoof : Float
    , qFloor : Float
    , qVent : Float
    , qBridges : Float
    , qTotal : Float
    }


calculate : Model -> Maybe Results
calculate m =
    String.toFloat m.totalFloorArea |> Maybe.andThen (\tfa ->
    String.toFloat m.numFloors      |> Maybe.andThen (\nf ->
    String.toFloat m.floorHeight    |> Maybe.andThen (\fh ->
    String.toFloat m.wallU          |> Maybe.andThen (\wu ->
    String.toFloat m.roofU          |> Maybe.andThen (\ru ->
    String.toFloat m.floorU         |> Maybe.andThen (\fu ->
    String.toFloat m.glazingPct     |> Maybe.andThen (\gp ->
    String.toFloat m.glazingU       |> Maybe.andThen (\gu ->
    String.toFloat m.yFactor        |> Maybe.andThen (\yf ->
    (effectiveACH m |> Maybe.andThen (\a -> if a >= 0 then Just a else Nothing)) |> Maybe.andThen (\ach ->
    String.toFloat m.tempIn         |> Maybe.andThen (\ti ->
    String.toFloat m.tempOut        |> Maybe.map     (\to_ ->
        let
            fp         = tfa / nf
            w          = sqrt fp
            eaveHeight = nf * fh

            roofGeom =
                case m.roofType of
                    FlatRoof ->
                        { roofArea = fp, gableArea = 0, vaultedVolume = 0, ridgeHeight = 0 }

                    VaultedRoof ->
                        let
                            pitch = toRad (clamp 1 89 (Maybe.withDefault 35 (String.toFloat m.pitchAngle)))
                            hr    = (w / 2) * tan pitch
                        in
                        { roofArea      = fp / cos pitch
                        , gableArea     = fp * tan pitch / 2
                        , vaultedVolume = fp * w * tan pitch / 4
                        , ridgeHeight   = hr
                        }

            roofArea      = roofGeom.roofArea
            gableArea     = roofGeom.gableArea
            vaultedVolume = roofGeom.vaultedVolume
            ridgeHeight   = roofGeom.ridgeHeight

            perimeter      = 4 * w
            rectWallArea   = perimeter * eaveHeight
            totalWallArea  = rectWallArea + gableArea
            glazingArea    = totalWallArea * (gp / 100)
            opaqueWallArea = totalWallArea - glazingArea

            volume = fp * eaveHeight + vaultedVolume

            -- Total exposed envelope area for y-factor
            totalExposedArea = opaqueWallArea + glazingArea + roofArea + fp
            htb              = yf * totalExposedArea

            deltaT = ti - to_

            qWalls   = wu  * opaqueWallArea    * deltaT
            qGlazing = gu  * glazingArea        * deltaT
            qRoof    = ru  * roofArea           * deltaT
            qFloor   = fu  * fp                 * deltaT
            qVent    = 0.33 * ach * volume      * deltaT
            qBridges = htb                      * deltaT
            qTotal   = qWalls + qGlazing + qRoof + qFloor + qVent + qBridges
        in
        { footprint          = fp
        , opaqueWallArea     = opaqueWallArea
        , glazingArea        = glazingArea
        , roofArea           = roofArea
        , gableArea          = gableArea
        , ridgeHeight        = ridgeHeight
        , totalExposedArea   = totalExposedArea
        , htb                = htb
        , volume             = volume
        , effectiveAch       = ach
        , deltaT             = deltaT
        , qWalls             = qWalls
        , qGlazing           = qGlazing
        , qRoof              = qRoof
        , qFloor             = qFloor
        , qVent              = qVent
        , qBridges           = qBridges
        , qTotal             = qTotal
        }
    ))))))))))))



-- UFH / HEAT PUMP


type alias HeatingResults =
    { specificOutput   : Float
    , maxOutput        : Float
    , coverage         : Float
    , requiredFlowTemp : Float
    , surfaceTemp      : Float
    , meanWaterTemp    : Float
    , designCop        : Float
    , scop             : Float
    , annualHeatKwh    : Float     -- gross, before solar gain
    , annualSolarGain  : Float     -- total incident through glazing
    , annualUsefulGain : Float     -- η × gain summed over months (η from ISO 13790)
    , annualExcessGain : Float     -- gain − useful (potential cooling load)
    , annualNetHeatKwh : Float     -- gross − useful
    , annualElecKwh    : Float     -- net / scop
    , effectiveHdd     : Float     -- user HDD scaled to setpoint
    , hddFractions     : List Float -- monthly HDD fractions at setpoint
    , annualInternalGain : Float   -- kWh/yr from internal gains (people, appliances)
    }


-- Fixed glazing orientation split: 35% N, 35% S, 15% E, 15% W
-- (EastWest in our Orientation type covers both E and W)
-- Shading factor (0.7) accounts for self-shading from the building's own
-- geometry (wings, eaves, reveals) plus typical surroundings (neighbours,
-- fences, trees). PVGIS factors assume an unobstructed vertical surface.
glazingVertFactor : Float
glazingVertFactor =
    0.7
        * (0.35 * tiltOrientFactor North 90
            + 0.35 * tiltOrientFactor South 90
            + 0.30 * tiltOrientFactor EastWest 90
          )


-- Real COP ~ 0.45 × Carnot between flow and source
estimateCop : Float -> Float -> Float
estimateCop flowC sourceC =
    let
        lift = Basics.max 1 (flowC - sourceC)
        carnot = (flowC + 273.15) / lift
    in
    0.45 * carnot


-- Floor surface upward heat transfer coeff (approx, EN 1264)
floorSurfaceH : Float
floorSurfaceH =
    10.8


calculateHeating : Model -> Results -> Maybe HeatingResults
calculateHeating m r =
    String.toFloat m.heatedFloorArea |> Maybe.andThen (\heatedFloorAreaM2 ->
    String.toFloat m.flowTemp        |> Maybe.andThen (\flowTempC ->
    String.toFloat m.emitterDeltaT   |> Maybe.andThen (\emitterDeltaTK ->
    String.toFloat m.hdd             |> Maybe.andThen (\hddCibseDegDays ->
    String.toFloat m.tempIn          |> Maybe.andThen (\indoorTempC ->
    String.toFloat m.tempOut         |> Maybe.andThen (\outdoorTempC ->
    String.toFloat m.gValue          |> Maybe.andThen (\gValue ->
    String.toFloat m.totalFloorArea  |> Maybe.andThen (\totalFloorAreaM2 ->
    String.toFloat m.pvIrradiation   |> Maybe.map     (\horizIrradKwhPerM2Yr ->
        let
            floorKwPerM2K         = floorCoeff m.floorCovering
            meanWaterTempC        = flowTempC - emitterDeltaTK / 2
            waterAboveIndoorK     = Basics.max 0 (meanWaterTempC - indoorTempC)
            specificOutputWperM2  = floorKwPerM2K * waterAboveIndoorK
            maxOutputW            = specificOutputWperM2 * heatedFloorAreaM2
            coveragePct           = if r.qTotal > 0 then maxOutputW / r.qTotal * 100 else 0
            surfaceTempC          = indoorTempC + specificOutputWperM2 / floorSurfaceH

            requiredSpecificWperM2 =
                if heatedFloorAreaM2 > 0 then r.qTotal / heatedFloorAreaM2 else 0

            requiredMeanWaterTempC =
                indoorTempC + (if floorKwPerM2K > 0 then requiredSpecificWperM2 / floorKwPerM2K else 0)

            requiredFlowTempC = requiredMeanWaterTempC + emitterDeltaTK / 2

            designCop = estimateCop flowTempC outdoorTempC
            scop      = estimateCop flowTempC 7

            -- Specific heat loss W/K from design calc
            specHeatLossWperK =
                if r.deltaT > 0 then r.qTotal / r.deltaT else 0

            -- User enters HDD at the CIBSE base (15.5°C) — a climate
            -- lookup for their location. We rescale to the setpoint using
            -- the monthly-mean temperatures, keeping HDD consistent with
            -- the setpoint input instead of a separate knob. Internal
            -- gains are handled below as a separate utilisation input,
            -- not via a base-temp offset — avoids double-counting solar.
            hddAtCibseBaseDegDays = hddAtBase cibseHddBase
            hddAtSetpointDegDays  = hddAtBase indoorTempC
            hddScaleFactor        =
                if hddAtCibseBaseDegDays > 0 then hddAtSetpointDegDays / hddAtCibseBaseDegDays else 1
            effectiveHddDegDays   = hddCibseDegDays * hddScaleFactor

            annualHeatKwh = specHeatLossWperK * effectiveHddDegDays * 24 / 1000

            monthlyHddFractions = hddFractionsAtBase indoorTempC

            -- Solar gain through glazing (annual)
            annualSolarGainKwh =
                r.glazingArea * gValue * horizIrradKwhPerM2Yr * glazingVertFactor

            -- Internal gains (people, appliances, DHW losses) — same
            -- 4 W/m² used by the cooling calc, distributed by days.
            annualInternalGainKwh =
                internalGainsWperM2 * totalFloorAreaM2 * 8760 / 1000

            internalMonthlyKwh =
                List.map (\d -> annualInternalGainKwh * d / 365) daysInMonth

            -- ISO 13790 utilisation factor: η × gain ≤ demand naturally,
            -- and excess (= total gain − useful) represents gains the
            -- heating system can't absorb (cooling-load potential in
            -- summer, or spill in shoulder months).
            tauHours = thermalMassTau m.thermalMass

            monthlyUsefulKwh =
                List.map3
                    (\hddFrac pvFrac internalKwh ->
                        let
                            grossHeatKwh = annualHeatKwh * hddFrac
                            gainKwh      = annualSolarGainKwh * pvFrac + internalKwh
                        in
                        if grossHeatKwh < 0.001 then
                            0

                        else
                            Basics.min grossHeatKwh
                                (utilisationFactor (gainKwh / grossHeatKwh) tauHours * gainKwh)
                    )
                    monthlyHddFractions
                    pvFractions
                    internalMonthlyKwh

            annualTotalGainKwh  = annualSolarGainKwh + annualInternalGainKwh
            annualUsefulGainKwh = List.sum monthlyUsefulKwh
            annualExcessGainKwh = Basics.max 0 (annualTotalGainKwh - annualUsefulGainKwh)
            annualNetHeatKwh    = Basics.max 0 (annualHeatKwh - annualUsefulGainKwh)
            annualElecKwh       = if scop > 0 then annualNetHeatKwh / scop else 0
        in
        { specificOutput   = specificOutputWperM2
        , maxOutput        = maxOutputW
        , coverage         = coveragePct
        , requiredFlowTemp = requiredFlowTempC
        , surfaceTemp      = surfaceTempC
        , meanWaterTemp    = meanWaterTempC
        , designCop        = designCop
        , scop             = scop
        , annualHeatKwh    = annualHeatKwh
        , annualSolarGain  = annualSolarGainKwh
        , annualUsefulGain = annualUsefulGainKwh
        , annualExcessGain = annualExcessGainKwh
        , annualNetHeatKwh = annualNetHeatKwh
        , annualElecKwh    = annualElecKwh
        , effectiveHdd     = effectiveHddDegDays
        , hddFractions     = monthlyHddFractions
        , annualInternalGain = annualInternalGainKwh
        }
    )))))))))



-- SOLAR PV


type alias PvResults =
    { pitch          : Float
    , factor         : Float
    , specificYield  : Float
    , annualKwh      : Float
    , pctOfHpElec    : Float
    }


pvPitch : Model -> Float
pvPitch m =
    case m.roofType of
        FlatRoof    -> 0
        VaultedRoof -> Maybe.withDefault 35 (String.toFloat m.pitchAngle)


calculatePv : Model -> HeatingResults -> Maybe PvResults
calculatePv m u =
    String.toFloat m.pvKwp         |> Maybe.andThen (\kwp ->
    String.toFloat m.pvIrradiation |> Maybe.map     (\h ->
        let
            pitch         = pvPitch m
            factor        = tiltOrientFactor m.pvOrientation pitch
            specificYield = h * factor * performanceRatio
            annualKwh     = kwp * specificYield
            pctOfHp       = if u.annualElecKwh > 0 then annualKwh / u.annualElecKwh * 100 else 0
        in
        { pitch         = pitch
        , factor        = factor
        , specificYield = specificYield
        , annualKwh     = annualKwh
        , pctOfHpElec   = pctOfHp
        }
    ))



-- COOLING (DESIGN DAY)


type alias CoolingResults =
    { deltaT        : Float
    , peakHoriz     : Float
    , qFabric       : Float   -- W, gain when Tout > Tin
    , qVent         : Float
    , qInternal     : Float
    , qSolar        : Float
    , qTotal        : Float
    }


calculateCooling : Model -> Results -> Maybe CoolingResults
calculateCooling m r =
    String.toFloat m.summerTempOut |> Maybe.andThen (\toS ->
    String.toFloat m.gValue        |> Maybe.andThen (\gVal ->
    String.toFloat m.totalFloorArea |> Maybe.andThen (\tfa ->
    String.toFloat m.pvIrradiation |> Maybe.map     (\horizIrr ->
        let
            tIn  = coolingSetpoint
            dT   = toS - tIn

            -- Reuse fabric/vent UA from the heating calc. Same physics
            -- — heat flows down a temperature gradient — sign just flips.
            -- Splitting fabric vs vent here for breakdown clarity.
            fabricW = r.qWalls + r.qGlazing + r.qRoof + r.qFloor + r.qBridges
            uaFabric = if r.deltaT > 0 then fabricW / r.deltaT else 0
            uaVent   = if r.deltaT > 0 then r.qVent / r.deltaT else 0

            qFabric   = uaFabric * dT
            qVent     = uaVent * dT
            qInternal = internalGainsWperM2 * tfa

            -- Peak solar through glazing. Reuses glazingVertFactor (annual
            -- ratio) as an approximation for peak — acceptable for sizing.
            -- South wall is slightly overstated since summer noon sun is
            -- high; east/west marginally understated. Net error is small
            -- given the 35/35/15/15 fixed split already averages directions.
            -- Internal blinds factor (0.6): typical mid-colour internal
            -- venetian/roller blinds transmit ~50-70% of incident solar;
            -- 0.6 is a reasonable mid-point. External shading would be
            -- ~0.2-0.3 but we assume internal only as the realistic default.
            peakHoriz = peakHorizFromAnnual horizIrr
            qSolar    = peakHoriz * r.glazingArea * gVal * glazingVertFactor * blindsFactor

            qTotal = Basics.max 0 (qFabric + qVent + qInternal + qSolar)
        in
        { deltaT    = dT
        , peakHoriz = peakHoriz
        , qFabric   = qFabric
        , qVent     = qVent
        , qInternal = qInternal
        , qSolar    = qSolar
        , qTotal    = qTotal
        }
    ))))



-- MONTHLY BREAKDOWN


type alias MonthlyRow =
    { month            : String
    , pvKwh            : Float
    , grossHeatKwh     : Float
    , solarGainKwh     : Float
    , usefulGainKwh    : Float
    , coolingKwh       : Float
    -- Day demand stack (kWh/day)
    , dayHpKwh         : Float
    , dayCoolKwh       : Float
    , dayHouseholdKwh  : Float
    , dayEvKwh         : Float
    , dayDhwKwh        : Float
    -- Night demand stack (kWh/day)
    , nightHpKwh       : Float
    , nightCoolKwh     : Float
    , nightHouseholdKwh : Float
    , nightEvKwh       : Float
    , nightDhwKwh      : Float
    -- Battery throughput (kWh/day). Day-discharge and night-discharge
    -- are mutually exclusive per month: if there's PV surplus we store
    -- it and discharge at night; if there's a day shortfall we charge
    -- overnight from off-peak grid and discharge during the day. Both
    -- fall out of the same cascade — no mode flag.
    , batteryDayDischargeKwh   : Float
    , batteryNightDischargeKwh : Float
    , batteryDayChargeFromPvKwh : Float
    , nightChargeFromGridKwh   : Float
    , exportKwh                : Float
    , curtailedKwh             : Float
    , dayImportKwh             : Float
    , nightImportKwh           : Float
    , daysInMonth              : Float
    }


monthNames : List String
monthNames =
    [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]


-- HDD integrated from monthly average temps at a given base temperature.
-- Using monthly means underestimates true HDD (Jensen's inequality) but
-- is good enough for a scaling factor.
hddAtBase : Float -> Float
hddAtBase base =
    List.map2 (\t d -> Basics.max 0 (base - t) * d) monthlyAvgTempC daysInMonth
        |> List.sum


-- Monthly HDD fractions derived at a given base, summing to 1.
hddFractionsAtBase : Float -> List Float
hddFractionsAtBase base =
    let
        contribs = List.map2 (\t d -> Basics.max 0 (base - t) * d) monthlyAvgTempC daysInMonth
        total = List.sum contribs
    in
    if total <= 0 then
        List.repeat 12 (1 / 12)
    else
        List.map (\c -> c / total) contribs


-- CIBSE standard UK base for published HDD values.
cibseHddBase : Float
cibseHddBase =
    15.5


-- PV generation fractions by month (UK south-facing, sums to 1.00)
pvFractions : List Float
pvFractions =
    [ 0.03, 0.05, 0.08, 0.12, 0.13, 0.14, 0.14, 0.12, 0.09, 0.06, 0.03, 0.01 ]


-- Average daylight hours by month (UK ~52°N)
daylightHours : List Float
daylightHours =
    [ 8.0, 10.0, 12.0, 14.0, 15.5, 16.5, 16.0, 14.5, 12.5, 11.0, 9.0, 7.5 ]


daysInMonth : List Float
daysInMonth =
    [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


monthlyBreakdown : Model -> Results -> HeatingResults -> PvResults -> List MonthlyRow
monthlyBreakdown m r u pv =
    let
        tau = thermalMassTau m.thermalMass
        tfa = String.toFloat m.totalFloorArea |> Maybe.withDefault 0

        -- Total UA (fabric + vent + bridges) for free-cooling estimate.
        ua = if r.deltaT > 0 then r.qTotal / r.deltaT else 0

        -- Daily internal gains (kWh/day), constant year-round.
        internalDaily = internalGainsWperM2 * tfa * 24 / 1000

        -- Monthly cooling target: use the user's heating internal design
        -- temp (e.g. 21°C) rather than the fixed 25°C cooling setpoint.
        -- The design cooling load card still uses 25°C (a more lenient
        -- comfort threshold typical for sizing); the monthly demand uses
        -- the tighter target to reflect occupants wanting consistent
        -- year-round indoor temperature.
        coolTarget = String.toFloat m.tempIn |> Maybe.withDefault coolingSetpoint

        -- Daily household + EV electricity (kWh/day). Household assumed
        -- evenly spread across the year; EV miles assumed evenly spread
        -- but with seasonally-varying Wh/mile.
        annualHousehold = String.toFloat m.householdElecKwh |> Maybe.withDefault 0
        householdDaily  = annualHousehold / 365
        annualEvMiles   = String.toFloat m.evMilesPerYear |> Maybe.withDefault 0
        milesPerDay     = annualEvMiles / 365

        batteryKwh = String.toFloat m.batteryKwh |> Maybe.withDefault 0

        -- DHW: annual thermal demand × monthly fraction ÷ dhwScop = kWh elec.
        -- Assumed scheduled overnight (off-peak) — common UK HP setup.
        annualDhw = String.toFloat m.annualDhwKwh |> Maybe.withDefault 0
    in
    List.map5
        (\name ( hddF, pvF, dhwF ) dl days avgT ->
            let
                grossHeat    = u.annualHeatKwh * hddF
                solarGain    = u.annualSolarGain * pvF
                internalGain = internalDaily * days
                gain         = solarGain + internalGain
                useful       =
                    if grossHeat < 0.001 then
                        0

                    else
                        Basics.min grossHeat
                            (utilisationFactor (gain / grossHeat) tau * gain)

                netHeat = Basics.max 0 (grossHeat - useful)
                netElec = if u.scop > 0 then netHeat / u.scop else 0
                dayFrac = dl / 24

                -- Monthly cooling, split day/night so that daytime fabric
                -- losses must absorb daytime gains — comfort must be
                -- maintained *during the day*, so night-time loss
                -- capacity doesn't subsidise day cooling. (Thermal mass
                -- shifting is ignored here; would partially close the gap
                -- for heavy buildings.)
                dayHours   = dl
                nightHours = 24 - dl

                dayT   = avgT + diurnalSwing
                nightT = avgT - diurnalSwing

                -- All solar excess hits during daylight. Split the total
                -- unused gain by its solar share so we don't double-count
                -- internal gains (which the cooling calc adds explicitly
                -- via internalDay/internalNight below).
                totalExcess    = Basics.max 0 (gain - useful)
                solarShare     = if gain > 0 then solarGain / gain else 0
                excessSolarDay = totalExcess * solarShare / days
                internalDay    = internalDaily * internalGainDayFrac
                internalNight  = internalDaily * (1 - internalGainDayFrac)

                freeCoolDay =
                    ua * Basics.max 0 (coolTarget - dayT) * dayHours / 1000

                freeCoolNight =
                    ua * Basics.max 0 (coolTarget - nightT) * nightHours / 1000

                coolingDayDemand   = Basics.max 0 (excessSolarDay + internalDay - freeCoolDay)
                coolingNightDemand = Basics.max 0 (internalNight - freeCoolNight)

                coolingDaily = coolingDayDemand + coolingNightDemand

                -- Cooling elec, split day/night per the demand split.
                coolDayElec   = coolingDayDemand / coolingScop
                coolNightElec = coolingNightDemand / coolingScop

                -- Heating elec day/night.
                hpDay   = netElec * dayFrac / days
                hpNight = netElec * (1 - dayFrac) / days

                -- Household elec day/night (60/40 split).
                householdDay   = householdDaily * internalGainDayFrac
                householdNight = householdDaily * (1 - internalGainDayFrac)

                -- EV daily kWh at the socket — wheel energy grossed up
                -- for charger + AC→DC losses.
                evDaily =
                    milesPerDay * evWhPerMile avgT / 1000 / evChargeEff

                -- DHW electricity per day (this month).
                dhwDaily = (annualDhw * dhwF) / (dhwScop * days)

                pvDaily = pv.annualKwh * pvF / days
                dayLoad = hpDay + coolDayElec + householdDay

                -- Single cascade: any PV surplus after day load flows
                -- EV → DHW → battery (direct-from-PV avoids round-trip
                -- losses). Any day shortfall is covered by the battery,
                -- which must have been charged overnight from off-peak
                -- grid. The two cases are mutually exclusive per month
                -- (surplus OR shortfall), so no mode flag is needed —
                -- both quantities fall out as zero in the other regime.
                pvSurplus    = Basics.max 0 (pvDaily - dayLoad)
                dayShortfall = Basics.max 0 (dayLoad - pvDaily)

                pvToEvDay        = Basics.min pvSurplus evDaily
                pvToDhwDay       = Basics.min (pvSurplus - pvToEvDay) dhwDaily

                evRemaining  = evDaily - pvToEvDay
                dhwRemaining = dhwDaily - pvToDhwDay

                nightLoadForBattery =
                    hpNight + coolNightElec + householdNight + evRemaining + dhwRemaining

                -- batteryKwh is usable (delivered) capacity. Charging
                -- input needed = capacity / efficiency. Round-trip losses
                -- are realised on the way out. Cap charge at what the
                -- night load can absorb — no point storing PV we can't
                -- discharge, it would just be export.
                batteryChargeCap = Basics.min batteryKwh nightLoadForBattery / batteryEff
                pvToBattery      = Basics.min (pvSurplus - pvToEvDay - pvToDhwDay) batteryChargeCap

                batteryNightDischarge = pvToBattery * batteryEff
                batteryDayDischarge   = Basics.min batteryKwh dayShortfall
                nightChargeFromGrid   = batteryDayDischarge / batteryEff

                -- Leftover PV after the direct-use cascade. Capped by the
                -- inverter's G98 export limit; anything above is curtailed
                -- (no export credit, no load to absorb it).
                exportAvailable = Basics.max 0 (pvSurplus - pvToEvDay - pvToDhwDay - pvToBattery)
                exportCapDaily  = exportLimitKw * dl
                exportKwh       = Basics.min exportAvailable exportCapDaily
                curtailedKwh    = exportAvailable - exportKwh

                -- Grid import = day shortfall left after battery cover,
                -- plus night load left after PV-stored battery, plus the
                -- off-peak battery top-up for winter days.
                dayImportKwh   = Basics.max 0 (dayLoad - pvDaily - batteryDayDischarge)
                nightLoadTotal = hpNight + coolNightElec + householdNight + evRemaining + dhwRemaining
                nightImportKwh = Basics.max 0 (nightLoadTotal - batteryNightDischarge) + nightChargeFromGrid
            in
            { month            = name
            , pvKwh            = pvDaily
            , grossHeatKwh     = Basics.max 0 (grossHeat - internalGain) / days
            , solarGainKwh     = solarGain / days
            , usefulGainKwh    = Basics.clamp 0 solarGain (useful - internalGain) / days
            , coolingKwh       = coolingDaily
            , dayHpKwh         = hpDay
            , dayCoolKwh       = coolDayElec
            , dayHouseholdKwh  = householdDay
            , dayEvKwh         = pvToEvDay
            , dayDhwKwh        = pvToDhwDay
            , nightHpKwh       = hpNight
            , nightCoolKwh     = coolNightElec
            , nightHouseholdKwh = householdNight
            , nightEvKwh       = evRemaining
            , nightDhwKwh      = dhwRemaining
            , batteryDayDischargeKwh   = batteryDayDischarge
            , batteryNightDischargeKwh = batteryNightDischarge
            , batteryDayChargeFromPvKwh = pvToBattery
            , nightChargeFromGridKwh   = nightChargeFromGrid
            , exportKwh                = exportKwh
            , curtailedKwh             = curtailedKwh
            , dayImportKwh             = dayImportKwh
            , nightImportKwh           = nightImportKwh
            , daysInMonth              = days
            }
        )
        monthNames
        (List.map3 (\a b c -> ( a, b, c )) u.hddFractions pvFractions dhwMonthlyFractions)
        daylightHours
        daysInMonth
        monthlyAvgTempC



-- COST
--
-- Tariffs in p/kWh; costs computed in £. Per-day quantities come from
-- the monthly sim; we multiply by days-in-month when aggregating.


type alias Tariffs =
    { dayRate : Float         -- p/kWh
    , nightRate : Float       -- p/kWh
    , exportRate : Float      -- p/kWh
    , standingCharge : Float  -- p/day
    }


readTariffs : Model -> Tariffs
readTariffs m =
    { dayRate        = String.toFloat m.dayRate        |> Maybe.withDefault 0
    , nightRate      = String.toFloat m.nightRate      |> Maybe.withDefault 0
    , exportRate     = String.toFloat m.exportRate     |> Maybe.withDefault 0
    , standingCharge = String.toFloat m.standingCharge |> Maybe.withDefault 0
    }


type alias CostRow =
    { dayImportKwh   : Float
    , nightImportKwh : Float
    , exportKwh      : Float
    , dayCost        : Float  -- £/day
    , nightCost      : Float  -- £/day
    , standingCost   : Float  -- £/day
    , exportCredit   : Float  -- £/day
    , netCost        : Float  -- £/day
    }


costForRow : Tariffs -> MonthlyRow -> CostRow
costForRow t row =
    let
        dayCost      = row.dayImportKwh   * t.dayRate        / 100
        nightCost    = row.nightImportKwh * t.nightRate      / 100
        standingCost =                      t.standingCharge / 100
        exportCredit = row.exportKwh      * t.exportRate     / 100
    in
    { dayImportKwh   = row.dayImportKwh
    , nightImportKwh = row.nightImportKwh
    , exportKwh      = row.exportKwh
    , dayCost        = dayCost
    , nightCost      = nightCost
    , standingCost   = standingCost
    , exportCredit   = exportCredit
    , netCost        = dayCost + nightCost + standingCost - exportCredit
    }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, -apple-system, sans-serif"
        , style "max-width" "900px"
        , style "margin" "2rem auto"
        , style "padding" "0 1.5rem"
        , style "color" "#1a1a2e"
        ]
        [ h1
            [ style "font-size" "1.5rem"
            , style "font-weight" "700"
            , style "margin-bottom" "0.25rem"
            ]
            [ text "Home Energy & Cost Modeller" ]
        , p
            [ style "color" "#666"
            , style "font-size" "0.9rem"
            , style "margin-bottom" "2rem"
            ]
            [ text "Heat loss, cooling, heat pump, hot water, PV, battery, EV and tariffs." ]
        , ukAssumptionsBox
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "2.5rem"
            , style "align-items" "start"
            ]
            [ inputsPanel model
            , resultsPanel model (calculate model)
            ]
        , monthlyChartSection model (calculate model)
        ]


monthlyChartSection : Model -> Maybe Results -> Html Msg
monthlyChartSection model maybeR =
    case maybeR |> Maybe.andThen (\r -> calculateHeating model r |> Maybe.andThen (\u -> calculatePv model u |> Maybe.map (\pv -> ( r, u, pv )))) of
        Nothing ->
            text ""

        Just ( r, u, pv ) ->
            let
                rows = monthlyBreakdown model r u pv
                tariffs = readTariffs model
                maxVal =
                    let
                        daySupply m = m.pvKwh + m.batteryDayDischargeKwh + m.dayImportKwh
                        dayD m = m.dayHpKwh + m.dayDhwKwh + m.dayCoolKwh + m.dayHouseholdKwh + m.dayEvKwh + m.batteryDayChargeFromPvKwh + m.exportKwh + m.curtailedKwh
                        nightSupply m = m.batteryNightDischargeKwh + m.nightImportKwh
                        nightD m = m.nightHpKwh + m.nightCoolKwh + m.nightHouseholdKwh + m.nightEvKwh + m.nightDhwKwh + m.nightChargeFromGridKwh
                    in
                    rows
                        |> List.map (\m -> List.maximum [ daySupply m, dayD m, nightSupply m, nightD m ] |> Maybe.withDefault 0)
                        |> List.maximum
                        |> Maybe.withDefault 1
            in
            div
                [ style "margin-top" "2rem"
                , style "background" "#f5f7ff"
                , style "border-radius" "10px"
                , style "padding" "1.25rem"
                ]
                [ sectionHeading "Monthly — Electricity Supply & Demand"
                , chartLegend
                , subHeading "Day (during daylight hours)"
                , dayNightChart rows maxVal True
                , subHeading "Night"
                , dayNightChart rows maxVal False
                , costChartSection rows tariffs
                , heatChartSection rows
                , coolingChartSection rows
                ]


ukAssumptionsBox : Html Msg
ukAssumptionsBox =
    details
        [ style "margin-bottom" "1.5rem"
        , style "padding" "0.75rem 1rem"
        , style "background" "#f5f7ff"
        , style "border" "1px solid #dde3f0"
        , style "border-radius" "8px"
        , style "font-size" "0.82rem"
        , style "color" "#444"
        ]
        [ summary
            [ style "cursor" "pointer"
            , style "font-weight" "600"
            , style "color" "#1a1a2e"
            ]
            [ text "UK-specific assumptions (climate, tariffs, efficiencies)" ]
        , ul [ style "margin" "0.6rem 0 0 0", style "padding-left" "1.2rem", style "line-height" "1.55" ]
            [ li [] [ text "Monthly outdoor temperatures, daylight hours and PV generation fractions are UK typical (Met Office normals, ~52°N)." ]
            , li [] [ text "HDD input is CIBSE-standard base 15.5 °C; the tool rescales it to your setpoint using monthly means. Internal gains (4 W/m², shared with the cooling calc) are applied separately via ISO 13790 utilisation." ]
            , li [] [ text "Square floor plan assumed for fabric heat-loss geometry." ]
            , li [] [ text "Peak horizontal irradiance estimated from annual horizontal irradiation; internal blinds (×0.6) assumed during peak cooling." ]
            , li [] [ text "Self-shading factor 0.7 applied to vertical glazing for heating solar gain." ]
            , li [] [ text "Heat pump SCOPs: space heating from flow temp (Carnot-derived); DHW 2.5; cooling 3.5." ]
            , li [] [ text "Battery round-trip efficiency 90%; EV charging efficiency 90%." ]
            , li [] [ text "EV energy use: 250 Wh/mi at 16.5 °C avg, 350 Wh/mi at 4.5 °C avg (linear)." ]
            , li [] [ text "Tariff model: flat day/night rates + export tariff + daily standing charge (no Agile/Flux time-of-use pricing)." ]
            , li [] [ text "Grid export capped at 3.68 kW (UK G98 single-phase limit); PV generated above this during daylight is curtailed." ]
            , li [] [ text "Battery dispatch: single daily cycle; direct-from-PV priority is day load → EV → DHW → battery. In months with no PV surplus, battery is charged overnight off-peak and discharged in the day." ]
            ]
        ]


sectionHeading : String -> Html Msg
sectionHeading label =
    p
        [ style "font-size" "0.72rem"
        , style "font-weight" "600"
        , style "text-transform" "uppercase"
        , style "letter-spacing" "0.08em"
        , style "color" "#888"
        , style "margin-bottom" "0.5rem"
        ]
        [ text label ]


subHeading : String -> Html Msg
subHeading label =
    p
        [ style "font-size" "0.8rem"
        , style "font-weight" "600"
        , style "color" "#1a1a2e"
        , style "margin" "1rem 0 0.5rem"
        ]
        [ text label ]


heatChartSection : List MonthlyRow -> Html Msg
heatChartSection rows =
    let
        maxHeat =
            rows
                |> List.map (\m -> Basics.max m.grossHeatKwh m.solarGainKwh)
                |> List.maximum
                |> Maybe.withDefault 1
    in
    div [ style "margin-top" "1.5rem" ]
        [ p
            [ style "font-size" "0.72rem"
            , style "font-weight" "600"
            , style "text-transform" "uppercase"
            , style "letter-spacing" "0.08em"
            , style "color" "#888"
            , style "margin-bottom" "0.5rem"
            ]
            [ text "Monthly — Heat Demand (net of internal gains) vs Solar Gain" ]
        , div
            [ style "display" "flex"
            , style "gap" "1rem"
            , style "flex-wrap" "wrap"
            , style "font-size" "0.78rem"
            , style "color" "#555"
            , style "margin-bottom" "0.75rem"
            ]
            [ legendSwatch "#c77700" "Heat demand (net of internal gains)"
            , legendSwatch "#e6b800" "Solar gain (dark = useful against heating)"
            ]
        , heatChart rows maxHeat
        ]


heatChart : List MonthlyRow -> Float -> Html Msg
heatChart rows maxVal =
    let
        chartH = 160.0

        bar colour h =
            div
                [ style "width" "14px"
                , style "height" (String.fromFloat h ++ "px")
                , style "background" colour
                ]
                []

        column row =
            let
                heatH    = row.grossHeatKwh * chartH / maxVal
                gainH    = row.solarGainKwh * chartH / maxVal
                usefulH  = row.usefulGainKwh * chartH / maxVal
                wastedH  = (row.solarGainKwh - row.usefulGainKwh) * chartH / maxVal
            in
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "gap" "0.4rem", style "flex" "1" ]
                [ div
                    [ style "display" "flex"
                    , style "align-items" "flex-end"
                    , style "gap" "3px"
                    , style "height" (String.fromFloat chartH ++ "px")
                    ]
                    [ bar "#c77700" heatH
                    , div
                        [ style "display" "flex"
                        , style "flex-direction" "column-reverse"
                        , style "height" (String.fromFloat gainH ++ "px")
                        , style "width" "14px"
                        ]
                        [ bar "#c49b00" usefulH
                        , bar "#f0d970" wastedH
                        ]
                    ]
                , div [ style "font-size" "0.72rem", style "color" "#666" ] [ text row.month ]
                ]

        yLabel v =
            div
                [ style "position" "absolute"
                , style "right" "0.5rem"
                , style "top" (String.fromFloat (chartH * (1 - v / maxVal)) ++ "px")
                , style "font-size" "0.7rem"
                , style "color" "#888"
                , style "transform" "translateY(-50%)"
                , style "white-space" "nowrap"
                , style "text-align" "right"
                ]
                [ text (String.fromInt (round v) ++ " kWh/day") ]
    in
    div [ style "display" "flex", style "gap" "0.25rem", style "padding-left" "3.5rem", style "position" "relative" ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" "3.5rem"
            , style "height" (String.fromFloat chartH ++ "px")
            ]
            [ yLabel maxVal
            , yLabel (maxVal / 2)
            , yLabel 0
            ]
        , div [ style "display" "flex", style "flex" "1", style "gap" "0.25rem" ]
            (List.map column rows)
        ]


coolingChartSection : List MonthlyRow -> Html Msg
coolingChartSection rows =
    let
        maxC =
            rows
                |> List.map .coolingKwh
                |> List.maximum
                |> Maybe.withDefault 0
    in
    if maxC < 0.01 then
        div [ style "margin-top" "1.5rem"
            , style "font-size" "0.8rem"
            , style "color" "#666"
            ]
            [ text "Cooling demand: ~0 across all months (gains absorbed by fabric)." ]

    else
        div [ style "margin-top" "1.5rem" ]
            [ p
                [ style "font-size" "0.72rem"
                , style "font-weight" "600"
                , style "text-transform" "uppercase"
                , style "letter-spacing" "0.08em"
                , style "color" "#888"
                , style "margin-bottom" "0.5rem"
                ]
                [ text "Monthly — Cooling Demand (excess gain after free cooling)" ]
            , div
                [ style "display" "flex"
                , style "gap" "1rem"
                , style "font-size" "0.78rem"
                , style "color" "#555"
                , style "margin-bottom" "0.75rem"
                ]
                [ legendSwatch "#3b82c4" "Cooling demand"
                ]
            , singleBarChart rows .coolingKwh "#3b82c4" maxC
            ]


costChartSection : List MonthlyRow -> Tariffs -> Html Msg
costChartSection rows tariffs =
    let
        monthly =
            rows
                |> List.map (\r -> ( r, costForRow tariffs r ))

        monthCost ( r, c ) = c.netCost * r.daysInMonth
        monthCosts = List.map monthCost monthly

        maxPos = monthCosts |> List.maximum |> Maybe.withDefault 0 |> Basics.max 0
        minNeg = monthCosts |> List.minimum |> Maybe.withDefault 0 |> Basics.min 0
        extent = Basics.max 1 (Basics.max maxPos (Basics.abs minNeg))
    in
    div [ style "margin-top" "1.5rem" ]
        [ sectionHeading "Monthly — Running Cost"
        , div
            [ style "display" "flex"
            , style "gap" "1rem"
            , style "flex-wrap" "wrap"
            , style "font-size" "0.78rem"
            , style "color" "#555"
            , style "margin-bottom" "0.75rem"
            ]
            [ legendSwatch "#c23b3b" "Net cost (import − export)"
            , legendSwatch "#2e7d32" "Net credit"
            ]
        , signedBarChart (List.map2 (\r c -> ( r.month, c )) rows monthCosts) extent
        ]


signedBarChart : List ( String, Float ) -> Float -> Html Msg
signedBarChart values extent =
    let
        halfH = 90.0
        chartH = halfH * 2

        column ( label, v ) =
            let
                h = Basics.abs v * halfH / extent
                colour = if v >= 0 then "#c23b3b" else "#2e7d32"
            in
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "gap" "0.3rem", style "flex" "1" ]
                [ div
                    [ style "position" "relative"
                    , style "width" "100%"
                    , style "height" (String.fromFloat chartH ++ "px")
                    ]
                    [ div
                        [ style "position" "absolute"
                        , style "left" "0", style "right" "0"
                        , style "top" (String.fromFloat halfH ++ "px")
                        , style "border-top" "1px solid #ccc"
                        ]
                        []
                    , div
                        [ style "position" "absolute"
                        , style "left" "50%"
                        , style "transform" "translateX(-50%)"
                        , style "width" "18px"
                        , style "height" (String.fromFloat h ++ "px")
                        , style "background" colour
                        , if v >= 0 then
                            style "top" (String.fromFloat (halfH - h) ++ "px")
                          else
                            style "top" (String.fromFloat halfH ++ "px")
                        ]
                        []
                    ]
                , div [ style "font-size" "0.72rem", style "color" "#666" ] [ text label ]
                ]

        yLabel v top =
            div
                [ style "position" "absolute"
                , style "right" "0.5rem"
                , style "top" (String.fromFloat top ++ "px")
                , style "font-size" "0.7rem"
                , style "color" "#888"
                , style "transform" "translateY(-50%)"
                , style "white-space" "nowrap"
                ]
                [ text ("£" ++ fmtMoney v) ]
    in
    div [ style "display" "flex", style "gap" "0.25rem", style "padding-left" "3.5rem", style "position" "relative" ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "width" "3.5rem"
            , style "height" (String.fromFloat chartH ++ "px")
            ]
            [ yLabel extent 0
            , yLabel 0 halfH
            , yLabel (-extent) chartH
            ]
        , div [ style "display" "flex", style "flex" "1", style "gap" "0.25rem" ]
            (List.map column values)
        ]


fmtMoney : Float -> String
fmtMoney v =
    let
        rounded = toFloat (round (v * 100)) / 100
    in
    String.fromFloat rounded


singleBarChart : List MonthlyRow -> (MonthlyRow -> Float) -> String -> Float -> Html Msg
singleBarChart rows getter colour maxVal =
    let
        chartH = 140.0

        column row =
            let
                h = getter row * chartH / Basics.max maxVal 0.001
            in
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "gap" "0.4rem", style "flex" "1" ]
                [ div
                    [ style "display" "flex"
                    , style "align-items" "flex-end"
                    , style "height" (String.fromFloat chartH ++ "px")
                    ]
                    [ div
                        [ style "width" "20px"
                        , style "height" (String.fromFloat h ++ "px")
                        , style "background" colour
                        ]
                        []
                    ]
                , div [ style "font-size" "0.72rem", style "color" "#666" ] [ text row.month ]
                ]

        yLabel v =
            div
                [ style "position" "absolute"
                , style "right" "0.5rem"
                , style "top" (String.fromFloat (chartH * (1 - v / Basics.max maxVal 0.001)) ++ "px")
                , style "font-size" "0.7rem"
                , style "color" "#888"
                , style "transform" "translateY(-50%)"
                , style "white-space" "nowrap"
                ]
                [ text (String.fromInt (round v) ++ " kWh/day") ]
    in
    div [ style "display" "flex", style "gap" "0.25rem", style "padding-left" "3.5rem", style "position" "relative" ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" "3.5rem"
            , style "height" (String.fromFloat chartH ++ "px")
            ]
            [ yLabel maxVal
            , yLabel (maxVal / 2)
            , yLabel 0
            ]
        , div [ style "display" "flex", style "flex" "1", style "gap" "0.25rem" ]
            (List.map column rows)
        ]


chartLegend : Html Msg
chartLegend =
    div
        [ style "display" "flex"
        , style "gap" "1rem"
        , style "flex-wrap" "wrap"
        , style "font-size" "0.78rem"
        , style "color" "#555"
        , style "margin-bottom" "0.75rem"
        ]
        [ legendSwatch "#2e7d32" "PV"
        , legendSwatch "#d4a017" "Battery"
        , legendSwatch "#c23b3b" "Grid import"
        , legendSwatch "#6fa96f" "Grid export"
        , legendSwatch "#cccccc" "Curtailed"
        , legendSwatch "#c77700" "HP heating"
        , legendSwatch "#b05577" "HP hot water"
        , legendSwatch "#3b82c4" "HP cooling"
        , legendSwatch "#888888" "Household"
        , legendSwatch "#444466" "EV"
        ]


legendSwatch : String -> String -> Html Msg
legendSwatch colour label =
    div [ style "display" "flex", style "align-items" "center", style "gap" "0.35rem" ]
        [ div
            [ style "width" "12px"
            , style "height" "12px"
            , style "background" colour
            , style "border-radius" "2px"
            ]
            []
        , text label
        ]


dayNightChart : List MonthlyRow -> Float -> Bool -> Html Msg
dayNightChart rows maxVal isDay =
    let
        chartH = 140.0
        barW = 14.0

        bar colour px =
            div
                [ style "width" (String.fromFloat barW ++ "px")
                , style "height" (String.fromFloat px ++ "px")
                , style "background" colour
                ]
                []

        h v = v * chartH / Basics.max maxVal 0.001

        stack segs =
            let
                total = List.sum (List.map Tuple.first segs)
            in
            div
                [ style "display" "flex"
                , style "flex-direction" "column-reverse"
                , style "height" (String.fromFloat (h total) ++ "px")
                , style "width" (String.fromFloat barW ++ "px")
                ]
                (List.map (\( v, c ) -> bar c (h v)) segs)

        supplyAndDemand row =
            if isDay then
                ( [ ( row.pvKwh,                   "#2e7d32" )
                  , ( row.batteryDayDischargeKwh,  "#d4a017" )
                  , ( row.dayImportKwh,            "#c23b3b" )
                  ]
                , [ ( row.dayHpKwh,                "#c77700" )
                  , ( row.dayDhwKwh,               "#b05577" )
                  , ( row.dayCoolKwh,              "#3b82c4" )
                  , ( row.dayHouseholdKwh,         "#888888" )
                  , ( row.dayEvKwh,                "#444466" )
                  , ( row.batteryDayChargeFromPvKwh, "#d4a017" )
                  , ( row.exportKwh,               "#6fa96f" )
                  , ( row.curtailedKwh,            "#cccccc" )
                  ]
                )

            else
                ( [ ( row.batteryNightDischargeKwh, "#d4a017" )
                  , ( row.nightImportKwh,           "#c23b3b" )
                  ]
                , [ ( row.nightHpKwh,            "#c77700" )
                  , ( row.nightDhwKwh,           "#b05577" )
                  , ( row.nightCoolKwh,          "#3b82c4" )
                  , ( row.nightHouseholdKwh,     "#888888" )
                  , ( row.nightEvKwh,            "#444466" )
                  , ( row.nightChargeFromGridKwh, "#d4a017" )
                  ]
                )

        column row =
            let
                ( supply, demand ) = supplyAndDemand row
            in
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "gap" "0.4rem", style "flex" "1" ]
                [ div
                    [ style "display" "flex"
                    , style "align-items" "flex-end"
                    , style "gap" "3px"
                    , style "height" (String.fromFloat chartH ++ "px")
                    ]
                    [ stack supply
                    , stack demand
                    ]
                , div [ style "font-size" "0.72rem", style "color" "#666" ] [ text row.month ]
                ]

        yLabel : Float -> Html Msg
        yLabel v =
            div
                [ style "position" "absolute"
                , style "right" "0.5rem"
                , style "top" (String.fromFloat (chartH * (1 - v / maxVal)) ++ "px")
                , style "font-size" "0.7rem"
                , style "color" "#888"
                , style "transform" "translateY(-50%)"
                , style "white-space" "nowrap"
                , style "text-align" "right"
                ]
                [ text (String.fromInt (round v) ++ " kWh/day") ]
    in
    div [ style "display" "flex", style "gap" "0.25rem", style "padding-left" "3.5rem", style "position" "relative" ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" "3.5rem"
            , style "height" (String.fromFloat chartH ++ "px")
            ]
            [ yLabel maxVal
            , yLabel (maxVal / 2)
            , yLabel 0
            ]
        , div [ style "display" "flex", style "flex" "1", style "gap" "0.25rem" ]
            (List.map column rows)
        ]


inputsPanel : Model -> Html Msg
inputsPanel m =
    div []
        [ inputSection "Building Geometry"
            [ inputRow "Total floor area" "m²" m.totalFloorArea SetTotalFloorArea "0" "5"
            , inputRow "Number of floors" "" m.numFloors SetNumFloors "1" "0.5"
            , inputRow "Floor-to-ceiling height" "m" m.floorHeight SetFloorHeight "0.1" "0.1"
            ]
        , roofSection m
        , inputSection "Fabric U-values"
            [ inputRow "Walls" "W/m²K" m.wallU SetWallU "0" "0.01"
            , inputRow "Roof" "W/m²K" m.roofU SetRoofU "0" "0.01"
            , inputRow "Floor" "W/m²K" m.floorU SetFloorU "0" "0.01"
            ]
        , inputSection "Glazing"
            [ inputRow "% of total wall area" "%" m.glazingPct SetGlazingPct "0" "1"
            , inputRow "Average U-value" "W/m²K" m.glazingU SetGlazingU "0" "0.1"
            , inputRow "g-value (solar)" "" m.gValue SetGValue "0" "0.05"
            ]
        , thermalMassSection m
        , yFactorSection m
        , ventilationSection m
        , inputSection "Design Temperatures"
            [ inputRow "Internal (heating)" "°C" m.tempIn SetTempIn "" "1"
            , inputRow "External (winter)"  "°C" m.tempOut SetTempOut "" "1"
            , inputRow "External (summer)"  "°C" m.summerTempOut SetSummerTempOut "" "1"
            ]
        , ufhSection m
        , inputSection "Annual Energy"
            [ inputRow "HDD (CIBSE, base 15.5 °C)" "°C·d" m.hdd SetHDD "0" "50"
            , inputRow "Hot water demand" "kWh/yr" m.annualDhwKwh SetAnnualDhwKwh "0" "100"
            ]
        , inputSection "Other Electricity"
            [ inputRow "Household baseload" "kWh/yr" m.householdElecKwh SetHouseholdElecKwh "0" "100"
            , inputRow "EV mileage" "mi/yr" m.evMilesPerYear SetEvMilesPerYear "0" "500"
            , inputRow "Home battery (usable)" "kWh" m.batteryKwh SetBatteryKwh "0" "1"
            ]
        , pvSection m
        , inputSection "Tariffs"
            [ inputRow "Day rate"        "p/kWh" m.dayRate        SetDayRate        "0" "1"
            , inputRow "Night rate"      "p/kWh" m.nightRate      SetNightRate      "0" "1"
            , inputRow "Export rate"     "p/kWh" m.exportRate     SetExportRate     "0" "1"
            , inputRow "Standing charge" "p/day" m.standingCharge SetStandingCharge "0" "5"
            ]
        ]


pvSection : Model -> Html Msg
pvSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Solar PV"
        , inputRow "Array size" "kWp" m.pvKwp SetPvKwp "0" "0.5"
        , inputRow "Horizontal irradiation" "kWh/m²/yr" m.pvIrradiation SetPvIrradiation "0" "10"
        , div [ style "margin-top" "0.4rem" ]
            [ p [ style "font-size" "0.75rem", style "color" "#888", style "margin-bottom" "0.3rem" ]
                [ text "Orientation" ]
            , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.2rem" ]
                [ radioRow (orientationLabel South)         (m.pvOrientation == South)         (SetPvOrientation South)
                , radioRow (orientationLabel SouthEastWest) (m.pvOrientation == SouthEastWest) (SetPvOrientation SouthEastWest)
                , radioRow (orientationLabel EastWest)      (m.pvOrientation == EastWest)      (SetPvOrientation EastWest)
                , radioRow (orientationLabel North)         (m.pvOrientation == North)         (SetPvOrientation North)
                ]
            ]
        ]


ufhSection : Model -> Html Msg
ufhSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Underfloor Heating + ASHP"
        , inputRow "Heated floor area" "m²" m.heatedFloorArea SetHeatedFloorArea "0" "5"
        , div [ style "margin-top" "0.4rem", style "margin-bottom" "0.4rem" ]
            [ p [ style "font-size" "0.75rem", style "color" "#888", style "margin-bottom" "0.3rem" ]
                [ text "Floor covering" ]
            , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.2rem" ]
                [ radioRow (floorLabel Tile)   (m.floorCovering == Tile)   (SetFloorCovering Tile)
                , radioRow (floorLabel Wood)   (m.floorCovering == Wood)   (SetFloorCovering Wood)
                , radioRow (floorLabel Carpet) (m.floorCovering == Carpet) (SetFloorCovering Carpet)
                ]
            ]
        , inputRow "Flow temperature" "°C" m.flowTemp SetFlowTemp "20" "1"
        , inputRow "Emitter ΔT" "K" m.emitterDeltaT SetEmitterDeltaT "1" "1"
        ]


roofSection : Model -> Html Msg
roofSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Roof"
        , modeToggle2
            ( "Flat", m.roofType == FlatRoof, SetRoofType FlatRoof )
            ( "Vaulted / pitched", m.roofType == VaultedRoof, SetRoofType VaultedRoof )
        , case m.roofType of
            FlatRoof    -> text ""
            VaultedRoof -> inputRow "Pitch angle" "°" m.pitchAngle SetPitchAngle "1" "1"
        ]


thermalMassSection : Model -> Html Msg
thermalMassSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Thermal Mass"
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.2rem" ]
            [ radioRow (thermalMassLabel LightMass)  (m.thermalMass == LightMass)  (SetThermalMass LightMass)
            , radioRow (thermalMassLabel MediumMass) (m.thermalMass == MediumMass) (SetThermalMass MediumMass)
            , radioRow (thermalMassLabel HeavyMass)  (m.thermalMass == HeavyMass)  (SetThermalMass HeavyMass)
            ]
        ]


yFactorSection : Model -> Html Msg
yFactorSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Thermal Bridges"
        , inputRow "y-factor" "W/m²K" m.yFactor SetYFactor "0" "0.01"
        , div
            [ style "display" "flex"
            , style "gap" "0.5rem"
            , style "flex-wrap" "wrap"
            , style "margin-top" "0.3rem"
            ]
            [ presetBtn "Accredited details (0.08)" "0.08" m.yFactor
            , presetBtn "No accredited details (0.15)" "0.15" m.yFactor
            , presetBtn "Passivhaus (0.04)" "0.04" m.yFactor
            ]
        ]


presetBtn : String -> String -> String -> Html Msg
presetBtn label val current =
    button
        [ onClick (SetYFactor val)
        , style "font-size" "0.75rem"
        , style "padding" "0.2rem 0.5rem"
        , style "border-radius" "4px"
        , style "cursor" "pointer"
        , style "border" ("1px solid " ++ (if current == val then "#1a1a2e" else "#ccc"))
        , style "background" (if current == val then "#1a1a2e" else "transparent")
        , style "color" (if current == val then "#fff" else "#666")
        ]
        [ text label ]


ventilationSection : Model -> Html Msg
ventilationSection m =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel "Airtightness (natural ventilation)"
        , modeToggle2
            ( "Direct ACH", m.ventMode == DirectACH, SetVentMode DirectACH )
            ( "n50 blower door", m.ventMode == FromN50, SetVentMode FromN50 )
        , case m.ventMode of
            DirectACH -> inputRow "Air change rate" "ACH" m.ach SetACH "0" "0.1"
            FromN50   -> n50Inputs m
        ]


n50Inputs : Model -> Html Msg
n50Inputs m =
    div []
        [ inputRow "n50 (blower door)" "ACH@50Pa" m.n50 SetN50 "0" "0.5"
        , div [ style "margin-top" "0.4rem", style "margin-bottom" "0.4rem" ]
            [ p [ style "font-size" "0.75rem", style "color" "#888", style "margin-bottom" "0.3rem" ]
                [ text "Conversion method" ]
            , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.2rem" ]
                [ radioRow "÷ 20  (rule of thumb)" (m.n50Method == RuleOfThumb) (SetN50Method RuleOfThumb)
                , radioRow "SAP 2012  (e = 0.07)"  (m.n50Method == SAP2012)     (SetN50Method SAP2012)
                , radioRow "Home Energy Model  (e = 0.079)" (m.n50Method == HEM) (SetN50Method HEM)
                ]
            ]
        , if m.n50Method /= RuleOfThumb then
            div [ style "margin-top" "0.4rem" ]
                [ p [ style "font-size" "0.75rem", style "color" "#888", style "margin-bottom" "0.3rem" ]
                    [ text "Shelter factor" ]
                , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.2rem" ]
                    [ radioRow (shelterLabel Exposed)  (m.shelterFactor == Exposed)  (SetShelterFactor Exposed)
                    , radioRow (shelterLabel Slight)   (m.shelterFactor == Slight)   (SetShelterFactor Slight)
                    , radioRow (shelterLabel Moderate) (m.shelterFactor == Moderate) (SetShelterFactor Moderate)
                    , radioRow (shelterLabel Heavy)    (m.shelterFactor == Heavy)    (SetShelterFactor Heavy)
                    ]
                ]
          else
            text ""
        ]



-- SHARED WIDGETS


sectionLabel : String -> Html Msg
sectionLabel title =
    p
        [ style "font-size" "0.72rem"
        , style "font-weight" "600"
        , style "text-transform" "uppercase"
        , style "letter-spacing" "0.08em"
        , style "color" "#888"
        , style "margin-bottom" "0.5rem"
        ]
        [ text title ]


inputSection : String -> List (Html Msg) -> Html Msg
inputSection title rows =
    div [ style "margin-bottom" "1.5rem" ]
        [ sectionLabel title
        , div [] rows
        ]


inputRow : String -> String -> String -> (String -> Msg) -> String -> String -> Html Msg
inputRow label unit val msg minVal stepVal =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.5rem"
        , style "margin-bottom" "0.4rem"
        ]
        [ span [ style "flex" "1", style "font-size" "0.88rem", style "color" "#333" ]
            [ text label ]
        , input
            [ type_ "number"
            , value val
            , onInput msg
            , Html.Attributes.min minVal
            , step stepVal
            , style "width" "80px"
            , style "padding" "0.3rem 0.5rem"
            , style "border" "1px solid #d0d0d0"
            , style "border-radius" "5px"
            , style "font-size" "0.88rem"
            , style "text-align" "right"
            , style "background" "#fafafa"
            ]
            []
        , span
            [ style "font-size" "0.78rem"
            , style "color" "#aaa"
            , style "width" "48px"
            , style "white-space" "nowrap"
            ]
            [ text unit ]
        ]


modeToggle2 : ( String, Bool, Msg ) -> ( String, Bool, Msg ) -> Html Msg
modeToggle2 ( lA, aA, mA ) ( lB, aB, mB ) =
    div
        [ style "display" "flex"
        , style "margin-bottom" "0.6rem"
        , style "border" "1px solid #d0d0d0"
        , style "border-radius" "6px"
        , style "overflow" "hidden"
        ]
        [ toggleBtn lA aA mA
        , toggleBtn lB aB mB
        ]


toggleBtn : String -> Bool -> Msg -> Html Msg
toggleBtn label active msg =
    button
        [ onClick msg
        , style "flex" "1"
        , style "padding" "0.35rem 0.5rem"
        , style "border" "none"
        , style "cursor" "pointer"
        , style "font-size" "0.82rem"
        , style "font-weight" (if active then "600" else "400")
        , style "background" (if active then "#1a1a2e" else "transparent")
        , style "color" (if active then "#fff" else "#555")
        , style "transition" "background 0.15s"
        ]
        [ text label ]


radioRow : String -> Bool -> Msg -> Html Msg
radioRow label selected msg =
    div
        [ onClick msg
        , style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.4rem"
        , style "cursor" "pointer"
        , style "font-size" "0.83rem"
        , style "color" (if selected then "#1a1a2e" else "#666")
        , style "font-weight" (if selected then "600" else "400")
        ]
        [ div
            [ style "width" "13px"
            , style "height" "13px"
            , style "border-radius" "50%"
            , style "border" ("2px solid " ++ (if selected then "#1a1a2e" else "#ccc"))
            , style "background" (if selected then "#1a1a2e" else "transparent")
            , style "flex-shrink" "0"
            ]
            []
        , text label
        ]



-- RESULTS


resultsPanel : Model -> Maybe Results -> Html Msg
resultsPanel model maybeR =
    case maybeR of
        Nothing ->
            div
                [ style "background" "#fff8e1"
                , style "border" "1px solid #ffe082"
                , style "border-radius" "10px"
                , style "padding" "1rem"
                , style "font-size" "0.88rem"
                , style "color" "#7a6000"
                ]
                [ text "Enter valid numbers in all fields to see results." ]

        Just r ->
            div []
                [ geometryCard model r
                , heatLossCard r
                , case calculateCooling model r of
                    Just c  -> coolingCard c
                    Nothing -> text ""
                , case calculateHeating model r of
                    Just u  -> ufhCard model u
                    Nothing -> text ""
                , case calculateHeating model r |> Maybe.andThen (\u -> calculatePv model u |> Maybe.map (\pv -> ( u, pv ))) of
                    Just ( u, pv ) ->
                        let
                            rows = monthlyBreakdown model r u pv
                            sumOver f = rows |> List.map (\row -> f row * row.daysInMonth) |> List.sum
                            annualCoolKwh   = sumOver .coolingKwh
                            annualCoolElec  = sumOver (\row -> row.dayCoolKwh + row.nightCoolKwh)
                            annualEvKwh     = sumOver (\row -> row.dayEvKwh + row.nightEvKwh)
                            annualHouseholdKwh = sumOver (\row -> row.dayHouseholdKwh + row.nightHouseholdKwh)
                            annualDhwElec = sumOver (\row -> row.dayDhwKwh + row.nightDhwKwh)
                            annualBatteryKwh = sumOver (\row -> row.batteryDayDischargeKwh + row.batteryNightDischargeKwh)
                            batteryCapacity = String.toFloat model.batteryKwh |> Maybe.withDefault 0
                            annualBatteryCycles =
                                if batteryCapacity > 0 then annualBatteryKwh / batteryCapacity else 0
                            tariffs = readTariffs model
                            costRows = List.map (\row -> ( row, costForRow tariffs row )) rows
                            annualDayImport   = costRows |> List.map (\( row, c ) -> c.dayImportKwh   * row.daysInMonth) |> List.sum
                            annualNightImport = costRows |> List.map (\( row, c ) -> c.nightImportKwh * row.daysInMonth) |> List.sum
                            annualExport      = costRows |> List.map (\( row, c ) -> c.exportKwh      * row.daysInMonth) |> List.sum
                            annualDayCost       = annualDayImport   * tariffs.dayRate        / 100
                            annualNightCost     = annualNightImport * tariffs.nightRate      / 100
                            annualStandingCost  = 365                * tariffs.standingCharge / 100
                            annualExportCredit  = annualExport      * tariffs.exportRate     / 100
                            annualNetCost       = annualDayCost + annualNightCost + annualStandingCost - annualExportCredit
                        in
                        div []
                            [ runningCard u annualCoolKwh annualCoolElec annualHouseholdKwh annualEvKwh annualDhwElec annualBatteryCycles
                            , costCard annualDayImport annualNightImport annualExport annualDayCost annualNightCost annualStandingCost annualExportCredit annualNetCost
                            ]

                    Nothing ->
                        text ""
                , case calculateHeating model r |> Maybe.andThen (calculatePv model) of
                    Just p  -> pvCard model p
                    Nothing -> text ""
                ]


coolingCard : CoolingResults -> Html Msg
coolingCard c =
    card "#f5f7ff" "Design Cooling Load"
        [ detailRow "ΔT (out − in)"        (fmt1 c.deltaT)              "K"
        , detailRow "Peak horiz. irrad."   (String.fromInt (round c.peakHoriz)) "W/m²"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.4rem 0" ] []
        , detailRow "Fabric gain"          (String.fromInt (round c.qFabric))   "W"
        , detailRow "Ventilation gain"     (String.fromInt (round c.qVent))     "W"
        , detailRow "Internal gains"       (String.fromInt (round c.qInternal)) "W"
        , detailRow "Solar (peak through glazing)" (String.fromInt (round c.qSolar)) "W"
        , hr [ style "border" "none", style "border-top" "2px solid #1a1a2e", style "margin" "0.6rem 0" ] []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "baseline"
            ]
            [ span [ style "font-weight" "700", style "font-size" "0.95rem" ] [ text "Total" ]
            , div [ style "text-align" "right" ]
                [ div
                    [ style "font-size" "1.5rem"
                    , style "font-weight" "700"
                    , style "color" "#1a1a2e"
                    , style "line-height" "1"
                    ]
                    [ text (fmt2 (c.qTotal / 1000) ++ " kW") ]
                , div [ style "font-size" "0.78rem", style "color" "#888", style "margin-top" "0.2rem" ]
                    [ text (String.fromInt (round c.qTotal) ++ " W") ]
                ]
            ]
        , p [ style "font-size" "0.75rem"
            , style "color" "#888"
            , style "margin-top" "0.5rem"
            ]
            [ text "Setpoint 25 °C. Internal gains 4 W/m². Solar assumes internal blinds (×0.6)." ]
        ]


pvCard : Model -> PvResults -> Html Msg
pvCard _ pv =
    card "#f5f7ff" "Solar PV"
        [ detailRow "Tilt"                (fmt1 pv.pitch)                            "°"
        , detailRow "Tilt/orient factor"  (fmt2 pv.factor)                           ""
        , detailRow "Specific yield"      (String.fromInt (round pv.specificYield))  "kWh/kWp/yr"
        , detailRow "Annual generation"   (String.fromInt (round pv.annualKwh))      "kWh/yr"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.6rem 0" ] []
        , detailRow "vs heat pump electricity" (fmt1 pv.pctOfHpElec) "%"
        , p [ style "font-size" "0.75rem"
            , style "color" "#888"
            , style "margin-top" "0.4rem"
            ]
          [ text "Note: PV generates mostly Apr–Sep, when the heat pump barely runs. Direct winter offset is much smaller than the annual %." ]
        ]


ufhCard : Model -> HeatingResults -> Html Msg
ufhCard model u =
    let
        coverageColor =
            if u.coverage >= 100 then "#2e7d32"
            else if u.coverage >= 85 then "#c77700"
            else "#c62828"

        surfaceWarn =
            u.surfaceTemp > 29

    in
    card "#f5f7ff" "UFH + Heat Pump"
        [ detailRow "Mean water temp"    (fmt1 u.meanWaterTemp)  "°C"
        , detailRow "Specific output"    (fmt1 u.specificOutput) "W/m²"
        , detailRow "Max UFH output"     (String.fromInt (round u.maxOutput)) "W"
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "font-size" "0.88rem"
            , style "margin-bottom" "0.3rem"
            , style "font-weight" "600"
            ]
            [ text "Coverage vs heat loss"
            , span [ style "color" coverageColor, style "font-variant-numeric" "tabular-nums" ]
                [ text (fmt1 u.coverage ++ " %") ]
            ]
        , detailRow "Required flow temp" (fmt1 u.requiredFlowTemp) "°C"
        , detailRow "Floor surface temp" (fmt1 u.surfaceTemp)      "°C"
        , if surfaceWarn then
            p [ style "font-size" "0.78rem"
              , style "color" "#c62828"
              , style "margin-top" "0.4rem"
              , style "margin-bottom" "0.2rem"
              ]
              [ text "⚠ Floor surface > 29 °C (comfort limit). Lower flow temp or add emitter area." ]
          else
            text ""
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.6rem 0" ] []
        , detailRow ("Design COP (source " ++ model.tempOut ++ " °C)") (fmt2 u.designCop) ""
        , detailRow "SCOP est. (source 7 °C)" (fmt2 u.scop) ""
        ]


runningCard : HeatingResults -> Float -> Float -> Float -> Float -> Float -> Float -> Html Msg
runningCard u annualCoolKwh annualCoolElec annualHouseholdKwh annualEvKwh annualDhwElec annualBatteryCycles =
    let
        totalElec = u.annualElecKwh + annualCoolElec + annualHouseholdKwh + annualEvKwh + annualDhwElec
    in
    card "#f5f7ff" "Annual Energy"
        [ detailRow "Effective HDD (rescaled to setpoint)" (String.fromInt (round u.effectiveHdd)) "°C·d"
        , detailRow "Gross heat demand"   (String.fromInt (round u.annualHeatKwh))     "kWh/yr"
        , detailRow "Solar gain (incident)" (String.fromInt (round u.annualSolarGain)) "kWh/yr"
        , detailRow "Internal gains (4 W/m²)" (String.fromInt (round u.annualInternalGain)) "kWh/yr"
        , detailRow "  → useful (heating)" ("−" ++ String.fromInt (round u.annualUsefulGain)) "kWh/yr"
        , detailRow "  → excess (cooling load)" (String.fromInt (round u.annualExcessGain)) "kWh/yr"
        , detailRow "Net heat demand"     (String.fromInt (round u.annualNetHeatKwh))  "kWh/yr"
        , detailRow "Cooling demand"      (String.fromInt (round annualCoolKwh))       "kWh/yr"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.4rem 0" ] []
        , detailRow "Electricity (heating)"  (String.fromInt (round u.annualElecKwh))   "kWh/yr"
        , detailRow ("Electricity (hot water, SCOP " ++ fmt1 dhwScop ++ ")")
                    (String.fromInt (round annualDhwElec)) "kWh/yr"
        , detailRow ("Electricity (cooling, SCOP " ++ fmt1 coolingScop ++ ")")
                    (String.fromInt (round annualCoolElec)) "kWh/yr"
        , detailRow "Electricity (household)" (String.fromInt (round annualHouseholdKwh)) "kWh/yr"
        , detailRow "Electricity (EV)"        (String.fromInt (round annualEvKwh))        "kWh/yr"
        , detailRow "Electricity (total)"     (String.fromInt (round totalElec))          "kWh/yr"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.4rem 0" ] []
        , detailRow "Battery cycles" (String.fromInt (round annualBatteryCycles)) "/yr"
        ]


costCard : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Html Msg
costCard dayImport nightImport export dayCost nightCost standingCost exportCredit netCost =
    card "#f5f7ff" "Annual Cost"
        [ detailRow "Day import"   (String.fromInt (round dayImport))   "kWh/yr"
        , detailRow "Night import" (String.fromInt (round nightImport)) "kWh/yr"
        , detailRow "PV export"    (String.fromInt (round export))      "kWh/yr"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.4rem 0" ] []
        , detailRow "Day cost"       ("£" ++ fmtMoney dayCost)           ""
        , detailRow "Night cost"     ("£" ++ fmtMoney nightCost)         ""
        , detailRow "Standing charge" ("£" ++ fmtMoney standingCost)     ""
        , detailRow "Export credit"  ("−£" ++ fmtMoney exportCredit)     ""
        , hr [ style "border" "none", style "border-top" "2px solid #1a1a2e", style "margin" "0.6rem 0" ] []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "baseline"
            ]
            [ span [ style "font-weight" "700", style "font-size" "0.95rem" ] [ text "Net annual cost" ]
            , div
                [ style "font-size" "1.5rem"
                , style "font-weight" "700"
                , style "color" (if netCost >= 0 then "#1a1a2e" else "#2e7d32")
                ]
                [ text
                    ((if netCost < 0 then "−£" else "£")
                        ++ fmtMoney (Basics.abs netCost)
                    )
                ]
            ]
        ]


geometryCard : Model -> Results -> Html Msg
geometryCard model r =
    card "#f5f7ff" "Geometry" <|
        [ detailRow "Footprint"    (fmt1 r.footprint)      "m²"
        , detailRow "Opaque walls" (fmt1 r.opaqueWallArea) "m²"
        , detailRow "Glazing"      (fmt1 r.glazingArea)    "m²"
        , detailRow "Roof"         (fmt1 r.roofArea)       "m²"
        ]
        ++ (case model.roofType of
                FlatRoof    -> []
                VaultedRoof ->
                    [ detailRow "  Gable walls"  (fmt1 r.gableArea)    "m²"
                    , detailRow "  Ridge height" (fmt2 r.ridgeHeight)  "m"
                    ]
           )
        ++
        [ detailRow "Total exposed area"     (fmt1 r.totalExposedArea) "m²"
        , detailRow "Thermal bridge coeff. (y × area)" (fmt1 r.htb) "W/K"
        , detailRow "Volume"                 (fmt1 r.volume)            "m³"
        , detailRow "Temperature difference" (fmt1 r.deltaT)            "K"
        , hr [ style "border" "none", style "border-top" "1px solid #dde3f0", style "margin" "0.4rem 0" ] []
        , case model.ventMode of
            DirectACH ->
                detailRow "Effective ACH" (fmt2 r.effectiveAch) "ACH"

            FromN50 ->
                div []
                    [ detailRow "n50"           model.n50             "ACH@50Pa"
                    , detailRow "Effective ACH" (fmt2 r.effectiveAch) "ACH"
                    , detailRow "Method"
                        (case model.n50Method of
                            RuleOfThumb -> "÷20"
                            SAP2012     -> "SAP 2012"
                            HEM         -> "Home Energy Model"
                        )
                        ""
                    ]
        ]


heatLossCard : Results -> Html Msg
heatLossCard r =
    card "#f5f7ff" "Heat Loss"
        [ lossRow "Walls"            r.qWalls   r.qTotal "#4a6fa5"
        , lossRow "Glazing"          r.qGlazing r.qTotal "#e07b39"
        , lossRow "Roof"             r.qRoof    r.qTotal "#5a9e6f"
        , lossRow "Floor"            r.qFloor   r.qTotal "#c8a020"
        , lossRow "Infiltration"      r.qVent    r.qTotal "#9b5ea2"
        , lossRow "Thermal bridges"  r.qBridges r.qTotal "#c44569"
        , hr [ style "border" "none", style "border-top" "2px solid #1a1a2e", style "margin" "0.75rem 0" ] []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "baseline"
            ]
            [ span [ style "font-weight" "700", style "font-size" "0.95rem" ] [ text "Total" ]
            , div [ style "text-align" "right" ]
                [ div
                    [ style "font-size" "1.75rem"
                    , style "font-weight" "700"
                    , style "color" "#1a1a2e"
                    , style "line-height" "1"
                    ]
                    [ text (fmt2 (r.qTotal / 1000) ++ " kW") ]
                , div [ style "font-size" "0.8rem", style "color" "#888", style "margin-top" "0.2rem" ]
                    [ text (String.fromInt (round r.qTotal) ++ " W") ]
                ]
            ]
        ]


card : String -> String -> List (Html Msg) -> Html Msg
card bg title children =
    div
        [ style "background" bg
        , style "border-radius" "10px"
        , style "padding" "1rem 1.25rem"
        , style "margin-bottom" "1rem"
        ]
        (p
            [ style "font-size" "0.72rem"
            , style "font-weight" "600"
            , style "text-transform" "uppercase"
            , style "letter-spacing" "0.08em"
            , style "color" "#888"
            , style "margin-bottom" "0.75rem"
            ]
            [ text title ]
            :: children
        )


detailRow : String -> String -> String -> Html Msg
detailRow label val unit =
    div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "font-size" "0.85rem"
        , style "color" "#555"
        , style "margin-bottom" "0.3rem"
        ]
        [ text label
        , span [ style "font-variant-numeric" "tabular-nums" ]
            [ text (if String.isEmpty unit then val else val ++ "\u{00A0}" ++ unit) ]
        ]


lossRow : String -> Float -> Float -> String -> Html Msg
lossRow label watts total colour =
    let
        pct = if total > 0 then Basics.max 0 (watts / total * 100) else 0
    in
    div [ style "margin-bottom" "0.75rem" ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "font-size" "0.88rem"
            , style "margin-bottom" "0.25rem"
            ]
            [ span [] [ text label ]
            , span [ style "font-variant-numeric" "tabular-nums", style "color" "#444" ]
                [ text (String.fromInt (round watts) ++ " W (" ++ fmt1 pct ++ "%)") ]
            ]
        , div
            [ style "background" "#e0e4f0"
            , style "border-radius" "4px"
            , style "height" "6px"
            , style "overflow" "hidden"
            ]
            [ div
                [ style "background" colour
                , style "height" "6px"
                , style "width" (String.fromFloat pct ++ "%")
                , style "border-radius" "4px"
                , style "transition" "width 0.2s ease"
                ]
                []
            ]
        ]



-- FORMATTING


fmt2 : Float -> String
fmt2 f =
    String.fromFloat (toFloat (round (f * 100)) / 100)


fmt1 : Float -> String
fmt1 f =
    String.fromFloat (toFloat (round (f * 10)) / 10)
