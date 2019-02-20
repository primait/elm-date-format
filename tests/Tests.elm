module Tests exposing (all)

{- A simple test an example of the library.
   Does not test every option, you can submit PRs for that.
-}

import Date
import Date.Format
import Expect
import String exposing (join, padLeft)
import Test exposing (..)
import Time
import Time.Format



-- test name, expected value, format string


all : Test
all =
    describe "Format tests" <|
        [ describe "Date Format tests" <|
            List.map (makeTest << formatDateTest) dateTestData
        , describe "Time Format tests" <|
            List.map (makeTest << formatTimeTest) timeTestData
        , describe "Date Format Year Padding" <|
            List.map (makeTest << formatDateYearPaddingTest) dateYearPaddingData
        ]


type alias TestTriple =
    ( String, String, String )


dateTestData : List TestTriple
dateTestData =
    [ ( "numeric date", "12/08/2014", "%d/%m/%Y" )
    , ( "spelled out date", "Tuesday, August 12, 2014", "%A, %B %d, %Y" )
    , ( "time", expectedTime, "%I:%M:%S %p" )
    , ( "time no spaces", expectedTimeNoSpace, "%H%M%S" )
    , ( "literal %", expectedTimeWithLiteral, "%H%%%M" )
    , ( "padding modifiers", "08|8| 8|08", "%m|%-m|%_m|%0m" )
    ]


timeTestData : List TestTriple
timeTestData =
    [ ( "time no spaces", expectedTimeNoSpace, "%H%M%S" )
    , ( "literal %", expectedTimeWithLiteral, "%H%%%M" )
    , ( "time colons", expectedTimeColons, "%H:%M" )
    , ( "time full colons", expectedFullTimeColons, "%H:%M:%S" )
    , ( "time with milliseconds", expectedTimeWithMilliSeconds, "%H:%M:%S:%L" )
    ]


dateYearPaddingData : List TestTriple
dateYearPaddingData =
    [ ( "year 4 digits padded with zeros", "0002-05-17", "%Y-%m-%d" )
    , ( "year 2 digits padded with zeros", "02-05-17", "%y-%m-%d" )
    , ( "year 4 digits padded with spaces", "   2-05-17", "%_Y-%m-%d" )
    , ( "year 2 digits padded with spaces", " 2-05-17", "%_y-%m-%d" )
    ]


expectedTimeWithLiteral =
    join "%" [ sampleHour, sampleMinute ]


expectedTimeNoSpace =
    join "" [ sampleHour, sampleMinute, sampleMinute ]


expectedTimeColons =
    join ":" [ sampleHour, sampleMinute ]


expectedFullTimeColons =
    join ":" [ sampleHour, sampleMinute, sampleSecond ]


expectedTimeWithMilliSeconds =
    join ":" [ sampleHour, sampleMinute, sampleSecond, sampleMilliSecond ]


expectedTime =
    join ":" [ sampleHour, sampleMinute, sampleMinute ]
        ++ (case Date.hour sampleDate < 12 of
                True ->
                    " AM"

                False ->
                    " PM"
           )


sampleDate : Date.Date
sampleDate =
    Date.fromTime 1407819233012


sampleTime : Time.Time
sampleTime =
    Date.toTime sampleDate


samplePaddingYearDate : Date.Date
samplePaddingYearDate =
    Result.withDefault sampleDate <| Date.fromString "0002-05-17"


pad : Int -> Int -> String
pad n =
    toString >> padLeft n '0'


sampleHour : String
sampleHour =
    Date.hour sampleDate
        |> pad 2


sampleMinute : String
sampleMinute =
    Date.minute sampleDate
        |> pad 2


sampleSecond : String
sampleSecond =
    Date.second sampleDate
        |> pad 2


sampleMilliSecond : String
sampleMilliSecond =
    Date.millisecond sampleDate
        |> pad 3


formatSampleDate : String -> String
formatSampleDate fstring =
    Date.Format.format fstring sampleDate


formatSampleTime : String -> String
formatSampleTime fstring =
    Time.Format.format fstring sampleTime


formatSamplePaddingYearDate : String -> String
formatSamplePaddingYearDate fstring =
    Date.Format.format fstring samplePaddingYearDate


formatDateTest : TestTriple -> TestTriple
formatDateTest ( a, b, format ) =
    ( a, b, formatSampleDate format )


formatTimeTest : TestTriple -> TestTriple
formatTimeTest ( a, b, format ) =
    ( a, b, formatSampleTime format )


formatDateYearPaddingTest : TestTriple -> TestTriple
formatDateYearPaddingTest ( a, b, format ) =
    ( a, b, formatSamplePaddingYearDate format )


makeTest : TestTriple -> Test
makeTest ( described, expected, actual ) =
    test described <| \() -> Expect.equal actual expected
