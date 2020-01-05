module Analyser.Checks.UnnecessaryLiteralBoolsTests exposing (all)

import Analyser.Checks.CheckTestUtil as CTU
import Analyser.Checks.UnnecessaryLiteralBools as UnnecessaryLiteralBools
import Analyser.Messages.Data as Data exposing (MessageData)
import Test exposing (Test, only)


redundantBoolean : ( String, String, List MessageData )
redundantBoolean =
    ( "redundantBoolean"
    , """module Bar exposing (..)

foo x =
    if condition then
        True

    else
        False

"""
    , [ Data.init "foo"
            |> Data.addRange "range"
                { start = { row = 4, column = 5 }, end = { row = 9, column = 1 } }
      ]
    )


redundantBooleanOperation : ( String, String, List MessageData )
redundantBooleanOperation =
    ( "redundantBooleanOperation"
    , """module Bar exposing (..)

foo =
    x == True

bar =
    False || foo
"""
    , [ Data.init "foo"
            |> Data.addRange "range"
                { start = { row = 7, column = 5 }, end = { row = 7, column = 17 } }
      , Data.init "foo"
            |> Data.addRange "range"
                { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
      ]
    )


singleRedundantBoolean : ( String, String, List MessageData )
singleRedundantBoolean =
    ( "singleRedundantBoolean"
    , """module Bar exposing (..)

foo x =
    if condition then
        x

    else
        False


bar x =
    if condition then
        True

    else
        x
"""
    , [ Data.init "foo"
            |> Data.addRange "range"
                { start = { row = 12, column = 5 }, end = { row = 17, column = 1 } }
      , Data.init "foo"
            |> Data.addRange "range"
                { start = { row = 4, column = 5 }, end = { row = 11, column = 1 } }
      ]
    )


all : Test
all =
    CTU.build "Analyser.Checks.UnnecessaryLiteralBools"
        UnnecessaryLiteralBools.checker
        [ redundantBoolean
        , redundantBooleanOperation
        , singleRedundantBoolean
        ]
