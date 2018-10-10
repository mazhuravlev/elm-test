module Tests exposing (..)

import Test exposing (..)
import Ttt exposing (checkWin)
import Expect



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!

winMatrix = [ 
         [1, 1, 1, 0, 0, 0, 0, 0, 0]
        ,[0, 0, 0, 1, 1, 1, 0, 0, 0]
        ,[0, 0, 0, 0, 0, 0, 1, 1, 1]
        ,[1, 0, 0, 1, 0, 0, 1, 0, 0]
        ,[0, 1, 0, 0, 1, 0, 0, 1, 0]
        ,[0, 0, 1, 0, 0, 1, 0, 0, 1]
        ,[1, 0, 0, 0, 1, 0, 0, 0, 1]
        ,[0, 0, 1, 0, 1, 0, 1, 0, 0]
        ]

loseMatrix = [ 
         [0, 0, 0, 0, 0, 0, 0, 0, 0]
        ,[1, 0, 0, 0, 0, 0, 0, 0, 0]
        ,[0, 1, 0, 0, 0, 0, 0, 0, 1]
        ,[0, 0, 0, 1, 0, 0, 1, 0, 0]
        ]


all : Test
all =
    describe "A Test Suite"
        [ test "Wins" <|
            \_ ->
                Expect.equal True (winMatrix |> List.map checkWin |> List.all (\x -> x))
        , test "Loses" <|
            \_ ->
                Expect.equal False (loseMatrix |> List.map checkWin |> List.any (\x -> x))
        ]
