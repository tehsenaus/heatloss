module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



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
    }


init : Model
init =
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


update : Msg -> Model -> Model
update msg model =
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


type alias UFHResults =
    { specificOutput   : Float
    , maxOutput        : Float
    , coverage         : Float
    , requiredFlowTemp : Float
    , surfaceTemp      : Float
    , meanWaterTemp    : Float
    , designCop        : Float
    , scop             : Float
    , annualHeatKwh    : Float
    , annualElecKwh    : Float
    }


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


calculateUFH : Model -> Results -> Maybe UFHResults
calculateUFH m r =
    String.toFloat m.heatedFloorArea |> Maybe.andThen (\hfa ->
    String.toFloat m.flowTemp        |> Maybe.andThen (\ft ->
    String.toFloat m.emitterDeltaT   |> Maybe.andThen (\edt ->
    String.toFloat m.hdd             |> Maybe.andThen (\hdd_ ->
    String.toFloat m.tempIn          |> Maybe.andThen (\ti ->
    String.toFloat m.tempOut         |> Maybe.map     (\to_ ->
        let
            k              = floorCoeff m.floorCovering
            meanWater      = ft - edt / 2
            dTemp          = Basics.max 0 (meanWater - ti)
            specificOutput = k * dTemp
            maxOutput      = specificOutput * hfa
            coverage       = if r.qTotal > 0 then maxOutput / r.qTotal * 100 else 0
            surfaceTemp    = ti + specificOutput / floorSurfaceH

            requiredSpecific =
                if hfa > 0 then r.qTotal / hfa else 0

            requiredMeanWater =
                ti + (if k > 0 then requiredSpecific / k else 0)

            requiredFlowTemp = requiredMeanWater + edt / 2

            designCop = estimateCop ft to_
            scop      = estimateCop ft 7

            -- Specific heat loss W/K from design calc
            specHeatLoss =
                if r.deltaT > 0 then r.qTotal / r.deltaT else 0

            annualHeatKwh = specHeatLoss * hdd_ * 24 / 1000
            annualElecKwh = if scop > 0 then annualHeatKwh / scop else 0
        in
        { specificOutput   = specificOutput
        , maxOutput        = maxOutput
        , coverage         = coverage
        , requiredFlowTemp = requiredFlowTemp
        , surfaceTemp      = surfaceTemp
        , meanWaterTemp    = meanWater
        , designCop        = designCop
        , scop             = scop
        , annualHeatKwh    = annualHeatKwh
        , annualElecKwh    = annualElecKwh
        }
    ))))))



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
            [ text "Heat Loss Calculator" ]
        , p
            [ style "color" "#666"
            , style "font-size" "0.9rem"
            , style "margin-bottom" "2rem"
            ]
            [ text "Fabric + ventilation heat loss. Assumes square floor plan." ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "2.5rem"
            , style "align-items" "start"
            ]
            [ inputsPanel model
            , resultsPanel model (calculate model)
            ]
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
            ]
        , yFactorSection m
        , ventilationSection m
        , inputSection "Design Temperatures"
            [ inputRow "Internal" "°C" m.tempIn SetTempIn "" "1"
            , inputRow "External" "°C" m.tempOut SetTempOut "" "1"
            ]
        , ufhSection m
        , inputSection "Annual Energy"
            [ inputRow "Heating degree days" "°C·d" m.hdd SetHDD "0" "50"
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
                , case calculateUFH model r of
                    Just u  -> ufhCard model u
                    Nothing -> text ""
                , case calculateUFH model r of
                    Just u  -> runningCard u
                    Nothing -> text ""
                ]


ufhCard : Model -> UFHResults -> Html Msg
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


runningCard : UFHResults -> Html Msg
runningCard u =
    card "#f5f7ff" "Annual Energy"
        [ detailRow "Heat demand"       (String.fromInt (round u.annualHeatKwh)) "kWh/yr"
        , detailRow "Electricity (HP)"  (String.fromInt (round u.annualElecKwh)) "kWh/yr"
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
