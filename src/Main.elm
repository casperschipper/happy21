module Main exposing
    ( interpolate
    , interpolateXY
    , line1d
    , main
    , offset
    , one
    , three
    , timeInterval
    , toPoints
    , two
    , zero
    )

import Basics.Extra exposing (fractionalModBy)
import Browser
import Browser.Dom as Dom
import Hex
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as D exposing (Decoder)
import Maybe.Extra as M
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions _ =
    Time.every 100 Tick


init _ =
    ( Model 0 0 0 0 0, Task.perform GotViewPort Dom.getViewport )


type alias Point =
    ( Float, Float )


type Msg
    = MouseClick MouseClickData
    | Touch TouchData
    | GotViewPort Dom.Viewport
    | Tick Time.Posix


type alias Model =
    { x : Int
    , y : Int
    , screenw : Int
    , screenh : Int
    , t : Float
    }


type alias MouseClickData =
    { mouseButton : Int
    , x : Int
    , y : Int
    }


type alias TouchData =
    { x : Int
    , y : Int
    }


decodeMousePos : Decoder MouseClickData
decodeMousePos =
    D.map3 MouseClickData
        (D.field "which" D.int)
        (D.field "pageX" D.int)
        (D.field "pageY" D.int)


decodeTouch : Decoder TouchData
decodeTouch =
    D.map2 TouchData
        (D.field "pageX" D.int)
        (D.field "pageY" D.int)


updateMaybe : a -> Maybe a -> a
updateMaybe old mNew =
    case mNew of
        Just new ->
            new

        Nothing ->
            old


getLeftMousePos : MouseClickData -> Maybe ( Int, Int )
getLeftMousePos data =
    case data.mouseButton of
        1 ->
            Just ( data.x, data.y )

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseClick data ->
            let
                mPos =
                    getLeftMousePos data
            in
            case mPos of
                Just ( x, y ) ->
                    ( { model | x = x, y = y }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Touch data ->
            ( { model
                | x = data.x
                , y = data.y
              }
            , Cmd.none
            )

        GotViewPort prt ->
            ( { model | screenw = prt.viewport.width |> floor, screenh = prt.viewport.height |> floor }, Cmd.none )

        Tick _ ->
            ( { model
                | t =
                    if model.t > toFloat model.screenw then
                        100

                    else
                        model.t + 5.0
              }
            , Cmd.none
            )


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
    [ ( 0, 0 )
    , ( 100, 0 )
    , ( 100, 100 )
    , ( 0, 100 )
    , ( 100, 100 )
    , ( 100, 200 )
    , ( 0, 200 )
    ]


one =
    [ ( 0, 50 )
    , ( 50, 0 )
    , ( 50, 200 )
    , ( 0, 200 )
    , ( 100, 200 )
    , ( 50, 200 )
    , ( 0, 200 )
    ]


toInts =
    List.map intify


type Time
    = Time Float


shiftTime : Float -> Time -> Time
shiftTime shift (Time t) =
    Time (t + shift)


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


timeSlice : Int -> Float -> Float -> List Time
timeSlice steps slice offs =
    let
        step =
            slice / toFloat steps

        aux curr =
            if curr <= slice then
                Time (curr + offs) :: aux (curr + step)

            else
                []
    in
    aux 0.0


linlin : Float -> Float -> Float -> Float -> Float -> Float
linlin a b c d x =
    ((x - b) / (b - a)) * (d - c) + c


timeWarp : Float -> List Time -> List Time
timeWarp power lst =
    let
        firstLast =
            case lst of
                (Time first) :: ts ->
                    case List.reverse ts of
                        (Time last) :: _ ->
                            Just ( first, last )

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case firstLast of
        Just ( first, last ) ->
            lst
                |> List.map
                    (\(Time x) ->
                        x
                            |> linlin first last 0 1
                            |> (^) power
                            |> linlin 0 1 first last
                            |> Time
                    )

        Nothing ->
            lst


timeExtraval : Int -> Float -> List Time
timeExtraval steps max =
    let
        step =
            1.0 / toFloat steps

        aux curr =
            if curr <= max then
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


offset : Float -> Float -> List ( Float, Float ) -> List ( Float, Float )
offset xOffset yOffset =
    List.map (\( x, y ) -> ( x + xOffset, y + yOffset ))


blackPolyline : Int -> Float -> List ( Int, Int ) -> Svg never
blackPolyline width opacity lst =
    polyline [ strokeWidth (String.fromInt width), fill "none", "black" |> stroke, strokeOpacity (String.fromFloat opacity), points (lst |> toPoints) ] []


morph : List ( Float, Float ) -> List ( Float, Float ) -> List Time -> List (List ( Float, Float ))
morph lstA lstB ts =
    let
        ( map, take ) =
            ( List.map, List.take )

        zipped =
            List.map2 Tuple.pair lstA lstB
    in
    map
        (\t ->
            map (\( apoint, bpoint ) -> interpolateXY apoint bpoint t) zipped
        )
        ts


fromChar : Char -> Maybe (List ( Float, Float ))
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


fromString : String -> List (List ( Float, Float ))
fromString str =
    String.toList str |> List.map fromChar |> M.values


offsets : Int -> Float -> Float -> List ( Float, Float )
offsets number spacing offsetY =
    List.range 0 number |> List.map (\x -> ( toFloat x * spacing, offsetY ))


line : ( Float, Float ) -> ( Float, Float ) -> List Time -> List ( Float, Float )
line a b ts =
    List.map (interpolateXY a b) ts


lineOffsetShapes : List ( Float, Float ) -> List (List ( Float, Float )) -> List (List ( Float, Float ))
lineOffsetShapes offsetLst shapes =
    List.map2 (\shape ( x, y ) -> offset x y shape) shapes offsetLst


line1d : List Time -> Float -> Float -> List Float
line1d ts a b =
    ts |> List.map (interpolate a b)


zeroPad str =
    case String.length str of
        1 ->
            "0" ++ str

        n ->
            str


sine : Float -> List Time -> List Float
sine cycles ts =
    List.map (\(Time t) -> sin (t * 3.14159265359 * cycles)) ts


triangle : Float -> Float
triangle x =
    (x * 2.0)
        |> (\a ->
                if a > 1.0 then
                    1.0 - (a - 1.0)

                else
                    a
           )


clip c =
    if c > 1.0 then
        1.0

    else
        c


scale : Float -> List ( Float, Float ) -> List ( Float, Float )
scale scaler =
    List.map (\( x, y ) -> ( x * scaler, y * scaler ))


layer x y w h lineWidth =
    let
        ( wf, hf ) =
            ( toFloat w, toFloat h )

        xf =
            toFloat x / toFloat w

        yf =
            toFloat y / toFloat h

        num =
            y // 10 |> Basics.clamp 10 1000

        ts =
            timeSlice num 0.1 0.0 |> List.map (shiftTime (xf * 0.9))

        rev b =
            1.0 - b

        tslice =
            timeSlice num 1.0 0.0

        ( map, concatMap ) =
            ( List.map, List.concatMap )

        oud =
            "-31-12-2020" |> fromString

        nieuw =
            "01-01-2021-" |> fromString

        charOffsets =
            offsets (List.length oud) 800 100

        perspective =
            line ( 0, 0 ) ( wf, hf * 10 ) ts |> List.map (\( xline, yline ) -> ( xline, fractionalModBy (hf * 3) yline ))

        trans =
            List.repeat num 0.8

        drawChar charA charB off transparancy =
            morph charA charB ts
                |> lineOffsetShapes perspective
                |> List.map (scale 1.0)
                |> map ((\( xx, yy ) -> offset xx yy) off)
                |> List.map (List.map intify)
                |> map (blackPolyline lineWidth transparancy)

        allChars =
            List.map4 drawChar oud nieuw charOffsets trans |> List.concat
    in
    allChars


view : Model -> Html Msg
view model =
    let
        ( x, y ) =
            ( model.x, model.y )

        w =
            model.screenw

        h =
            model.screenh
    in
    Html.div
        [ Events.on "mousemove" (D.map MouseClick decodeMousePos)
        , Events.on "touchmove" (D.map Touch decodeTouch)
        ]
        [ Html.p []
            [ Html.text <|
                (x |> String.fromInt)
                    ++ " "
                    ++ String.fromInt y
                    ++ "x/w: "
                    ++ String.fromFloat (toFloat x / toFloat w)
            ]
        , svg
            [ width (String.fromInt w)
            , height (String.fromInt h)
            , viewBox "0 0 10000 5000"
            ]
            (layer x y w h 8)
        ]
