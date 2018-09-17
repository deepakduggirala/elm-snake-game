module Grid exposing
    ( GridPoint
    , GridUnit
    , Orientation(..)
    , Vector
    , arePerpendicular
    , distance
    , distance_in_grid
    , doIntersect
    , gridPointEqual
    , gridResolution
    , interpolate
    , onSameAxis
    , onSegment
    , orientation
    , origin
    , scaleFrom
    , scalePointFrom
    , scalePointTo
    , scaleTo
    , sign
    , translate
    , turn_point
    )


type alias GridUnit =
    Int


type alias GridPoint =
    ( GridUnit, GridUnit )


interpolate : GridPoint -> GridPoint -> Int -> GridPoint
interpolate a b n =
    if onSameAxis a b then
        let
            ( x1, y1 ) =
                a

            ( x2, y2 ) =
                b
        in
        ( x1 + sign (x2 - x1) * n
        , y1 + sign (y2 - y1) * n
        )

    else
        let
            c =
                turn_point a b

            d13 =
                distance_in_grid a c
        in
        if n > d13 then
            interpolate c b (n - d13)

        else
            interpolate a c n


turn_point : GridPoint -> GridPoint -> GridPoint
turn_point ( x1, y1 ) ( x2, y2 ) =
    ( x1, y2 )


distance =
    distance_in_grid


distance_in_grid : GridPoint -> GridPoint -> Int
distance_in_grid ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


onSameAxis : GridPoint -> GridPoint -> Bool
onSameAxis ( x1, y1 ) ( x2, y2 ) =
    x1 == x2 || y1 == y2


gridPointEqual : GridPoint -> GridPoint -> Bool
gridPointEqual ( x1, y1 ) ( x2, y2 ) =
    x1 == x2 && y1 == y2


arePerpendicular : ( GridPoint, GridPoint ) -> ( GridPoint, GridPoint ) -> Bool
arePerpendicular ( ( x1, y1 ), ( x2, y2 ) ) ( ( x3, y3 ), ( x4, y4 ) ) =
    (x1 - x2) * (x3 - x4) == 0 && (y1 - y2) * (y3 - y4) == 0



-- is r on segment pq?


onSegment : GridPoint -> GridPoint -> GridPoint -> Bool
onSegment ( px, py ) ( qx, qy ) ( rx, ry ) =
    qx
        <= max px rx
        && qx
        >= min px rx
        && qy
        <= max py ry
        && qy
        >= min py ry


type Orientation
    = Clockwise
    | Counterclockwise
    | Colinear


orientation : GridPoint -> GridPoint -> GridPoint -> Orientation
orientation ( px, py ) ( qx, qy ) ( rx, ry ) =
    let
        val =
            (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
    in
    if val == 0 then
        Colinear

    else if val > 0 then
        Clockwise

    else
        Counterclockwise


doIntersect : ( GridPoint, GridPoint ) -> ( GridPoint, GridPoint ) -> Bool
doIntersect ( p1, q1 ) ( p2, q2 ) =
    let
        o1 =
            orientation p1 q1 p2

        o2 =
            orientation p1 q1 q2

        o3 =
            orientation p2 q2 p1

        o4 =
            orientation p2 q2 q1
    in
    if o1 /= o2 && o3 /= o4 then
        True

    else
        -- Special Cases p1, q1 and p2 are colinear and p2 lies on segment p1q1  and other cyclic variants
        o1
            == Colinear
            && onSegment p1 p2 q1
            || o2
            == Colinear
            && onSegment p1 q2 q1
            || o3
            == Colinear
            && onSegment p2 q1 q2
            || o4
            == Colinear
            && onSegment p2 q1 q2


sign : Int -> Int
sign x =
    if x < 0 then
        -1

    else if x == 0 then
        0

    else
        1


scaleTo : Float -> GridUnit
scaleTo x =
    truncate (x * (2 ^ 32) / 100)


scaleFrom : GridUnit -> Float
scaleFrom x =
    toFloat x * 100 / (2 ^ 32)


scalePointTo : Float -> ( Float, Float ) -> GridPoint
scalePointTo s ( x, y ) =
    ( truncate <| x * s, truncate <| y * s )


scalePointFrom : Float -> GridPoint -> ( Float, Float )
scalePointFrom s ( x, y ) =
    ( toFloat x * s, toFloat y * s )


type alias Vector =
    ( GridPoint, GridPoint )


origin : GridPoint
origin =
    ( 0, 0 )


translate : GridPoint -> Vector -> GridPoint
translate ( px, py ) ( ( x1, y1 ), ( x2, y2 ) ) =
    ( px + (x2 - x1), py + (y2 - y1) )


gridResolution : ( Int, Int ) -> ( Int, Int )
gridResolution ( sw, sh ) =
    let
        gw =
            2 ^ 32
    in
    ( gw, truncate <| toFloat gw * (toFloat sh / toFloat sw) )
