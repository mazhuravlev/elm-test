module Ttt exposing (checkWin)

import List.Extra exposing (zip)

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

checkWin: List Int -> Bool
checkWin test =
    List.any checkSubset (List.map (zip test) winMatrix)

checkSubset: (List (Int, Int)) -> Bool
checkSubset list =
  List.all winChecker list 

winChecker: (Int, Int) -> Bool
winChecker (test, actual) =
  case actual of
   1 -> if test == 1 then True else False
   _ -> True