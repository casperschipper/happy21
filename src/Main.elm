module Main exposing
    ( interpolate
    , interpolateXY
    , main
    , offset
    , one
    , polyShapeAt
    , three
    , timeInterval
    , toPoints
    , two
    , zero
    )

import Svg exposing (..)
import Svg.Attributes exposing (..)


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


timeInterval steps =
    let
        step =
            1.0 / steps

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


polyShapeAt : List ( Int, Int ) -> Int -> Int -> String
polyShapeAt shape offX offY =
    shape |> offset offX offY |> toPoints

morph : List (Int,Int) -> Int -> List (Int,Int)
morph lst steps =
    ts = timeInterval

main =
    let
        twoString =
            toPoints two
    in
    svg
        [ width "800"
        , height "800"
        , viewBox "0 0 800 800"
        ]
        [ polyline [ fill "none", stroke "black", points twoString ] []
        ]
