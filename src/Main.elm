module Main exposing
    ( interpolate
    , interpolateXY
    , main
    , offset
    , one
    , three
    , timeInterval
    , toPoints
    , two
    , zero
    )

import Maybe.Extra as M
import Svg exposing (..)
import Svg.Attributes exposing (..)


dash =
    [ ( 0, 100 )
    , ( 100, 100 )
    , ( 0, 100 )
    , ( 100, 100 )
    , ( 0, 100 )
    , ( 100, 100 )
    , ( 0, 100 )
    ]


two =
    [ ( 0, 0 )
    , ( 100, 0 )
    , ( 100, 100 )
    , ( 0, 100 )
    , ( 0, 200 )
    , ( 100, 200 )
    , ( 100, 200 )
    ]


zero =
    [ ( 0, 0 )
    , ( 100, 0 )
    , ( 100, 200 )
    , ( 0, 200 )
    , ( 0, 0 )
    , ( 100, 0 )
    , ( 100, 200 )
    ]


three =
    [ ( 100, 0 )
    , ( 0, 0 )
    , ( 0, 100 )
    , ( 100, 100 )
    , ( 0, 100 )
    , ( 0, 200 )
    , ( 100, 200 )
    ]


one =
    [ ( 50, 0 )
    , ( 50, 200 )
    , ( 50, 0 )
    , ( 50, 200 )
    , ( 50, 0 )
    , ( 50, 200 )
    , ( 50, 0 )
    ]


toInts : List ( Float, Float ) -> List ( Int, Int )
toInts =
    List.map (Tuple.mapBoth floor floor)


type Time
    = Time Float


timeInterval : Int -> List Time
timeInterval steps =
    let
        step =
            1.0 / toFloat steps

        aux curr =
            if curr <= 1.0 then
                Time curr :: aux (curr + step)

            else
                []
    in
    aux 0.0


interpolate : Float -> Float -> Time -> Float
interpolate x1 x2 (Time t) =
    x1 + ((x2 - x1) * t)


interpolateXY : ( Float, Float ) -> ( Float, Float ) -> Time -> ( Float, Float )
interpolateXY ( x1, y1 ) ( x2, y2 ) t =
    ( interpolate x1 x2 t, interpolate y1 y2 t )


intify : ( Float, Float ) -> ( Int, Int )
intify ( x, y ) =
    ( round x, round y )


floatify : ( Int, Int ) -> ( Float, Float )
floatify ( x, y ) =
    ( toFloat x, toFloat y )


toPoints : List ( Int, Int ) -> String
toPoints =
    let
        f =
            String.fromInt
    in
    List.map (\( x, y ) -> f x ++ "," ++ f y) >> String.join " "


offset : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
offset xOffset yOffset =
    List.map (\( x, y ) -> ( x + xOffset, y + yOffset ))


blackPolyline : List ( Int, Int ) -> Svg never
blackPolyline lst =
    polyline [ fill "none", stroke "black", points (lst |> toPoints) ] []


morph : List ( Int, Int ) -> List ( Int, Int ) -> Int -> List (List ( Int, Int ))
morph lstA lstB steps =
    let
        map =
            List.map

        ts =
            timeInterval steps

        ( lstAf, lstBf ) =
            ( map floatify lstA, map floatify lstB )

        zipped =
            List.map2 Tuple.pair lstAf lstBf
    in
    map
        (\t ->
            map (\( apoint, bpoint ) -> interpolateXY apoint bpoint t) zipped |> map intify
        )
        ts


fromChar : Char -> Maybe (List ( Int, Int ))
fromChar c =
    case c of
        '1' ->
            Just one

        '3' ->
            Just three

        '2' ->
            Just two

        '0' ->
            Just zero

        '-' ->
            Just dash

        _ ->
            Nothing


fromString : String -> List (List ( Int, Int ))
fromString str =
    String.toList str |> List.map fromChar |> M.values


offsets : Int -> Int -> Int -> List ( Int, Int )
offsets number spacing offsetY =
    List.range 0 number |> List.map (\x -> ( x * spacing, offsetY ))


line : ( Int, Int ) -> ( Int, Int ) -> List Time -> List ( Int, Int )
line a b ts =
    List.map (interpolateXY (floatify a) (floatify b)) ts |> List.map intify


lineOffsetShapes : List ( Int, Int ) -> List (List ( Int, Int )) -> List (List ( Int, Int ))
lineOffsetShapes offsetLst shapes =
    List.map2 (\shape ( x, y ) -> offset x y shape) shapes offsetLst


main =
    let
        twoString =
            toPoints two

        ( map, concatMap ) =
            ( List.map, List.concatMap )

        -- morphed =
        --     morph three zero 32 |> map (offset 100 100) |> map blackPolyline
        oud =
            "31-12-2020" |> fromString

        nieuw =
            "01-01-2021" |> fromString

        charOffsets =
            offsets 10 150 100

        perspective =
            line ( 0, 0 ) ( 100, 100 ) (timeInterval 30)

        drawChar charA charB off =
            morph charA charB 30
                |> lineOffsetShapes perspective
                |> map ((\( x, y ) -> offset x y) off)
                |> map blackPolyline

        allChars =
            List.map3 drawChar oud nieuw charOffsets |> List.concat
    in
    svg
        [ width "1500"
        , height "1500"
        , viewBox "0 0 1500 1500"
        ]
        allChars
