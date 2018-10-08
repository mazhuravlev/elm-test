module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Debug exposing (..)
import Browser
import List.Extra exposing (zip)

-- APP
init: (Model, Cmd Msg)
init = ({cells = List.range 0 8 |> List.map (\x -> EmptyCell x), turn = X, winner = Nothing}, Cmd.none)

type Turn = X | O

type Msg = CellClick Int | Reset

type Cell = EmptyCell Int | XCell Int | OCell Int

type alias Model = {cells: List Cell, turn: Turn, winner: Maybe Turn}

cellView: Bool -> Turn -> Cell -> Html Msg
cellView hasWinner turn cell =
    let 
      emptyClass = 
        case turn of
          X -> "cell cell-x cell-ghost"
          O -> "cell cell-o cell-ghost"
      cellAttrs cellIndex = if hasWinner then [class "cell"] else [class emptyClass, onClick (CellClick cellIndex)]
    in
    case cell of 
      EmptyCell i -> div (cellAttrs i) []
      XCell i -> div [class "cell cell-x"] []
      OCell i -> div [class "cell cell-o"] []

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CellClick cellIndex -> ({
        model | 
        turn = switchTurn model.turn
        ,cells = List.map (applyClick cellIndex model.turn) model.cells
        ,winner = detectWinner (List.map (applyClick cellIndex model.turn) model.cells)
        }, Cmd.none)
    Reset -> init

matrix: List(List Int)
matrix = [ 
        [1, 1, 1, 0, 0, 0, 0, 0, 0]
        ,[0, 0, 0, 1, 1, 1, 0, 0, 0]
        ,[0, 0, 0, 0, 0, 0, 1, 1, 1]
        ,[1, 0, 0, 1, 0, 0, 1, 0, 0]
        ,[0, 1, 0, 0, 1, 0, 0, 1, 0]
        ,[0, 0, 1, 0, 0, 1, 0, 0, 1]
        ,[1, 0, 0, 0, 1, 0, 0, 0, 1]
        ,[0, 0, 1, 0, 1, 0, 1, 0, 0]
        ]

win: List Int -> Bool
win test =
    List.any checkSubset (List.map (zip test) matrix)
    --List.any (\r -> r == test) matrix

checkSubset: (List (Int, Int)) -> Bool
checkSubset list =
  List.all winChecker list 

winChecker: (Int, Int) -> Bool
winChecker (test, actual) =
  case actual of
   1 -> if test == 1 then True else False
   _ -> True

detectWinner: List Cell -> Maybe Turn
detectWinner cells =
  let 
    xmapper c =
      case c of 
        XCell i -> 1
        _ -> 0
    omapper c =
      case c of 
        OCell i -> 1
        _ -> 0
    xVec = List.map xmapper cells    
  in
    if win xVec then Just X else if win (List.map omapper cells) then Just O else Nothing

switchTurn: Turn -> Turn
switchTurn turn =
   case turn of
      X -> O
      O -> X

applyClick: Int -> Turn -> Cell -> Cell
applyClick cellIndex turn =
  let 
    applyer c = 
      case c of
        EmptyCell i ->
          if i /= cellIndex then EmptyCell i else 
            case turn of
              X -> XCell i
              O -> OCell i
        XCell i -> XCell i
        OCell i -> OCell i
  in
  applyer

turnToString: Turn -> String
turnToString turn = 
  case turn of
    X -> "X"
    O -> "O"

-- VIEW
view : Model -> Html Msg
view model =
  let
    hasWinner = 
      case model.winner of
       Nothing -> False
       _ -> True
    cells = model.cells |> List.map (cellView hasWinner model.turn)
    msg = 
      case model.winner of
        Just X -> "Winner is X!"
        Just O -> "Winner is O!"
        Nothing -> "Current turn: " ++ (turnToString model.turn)
  in
  -- style [("margin-top", "30px"), ( "text-align", "center" )
  div [ class "container"] [
    div [ class "row" ][
      div [ class "col-xs-12" ][
        div [ class "jumbotron" ] [
          h1 [] [text msg]
          ,div [class "field"] cells
          ,button [onClick Reset, class "btn btn-primary btn-block"] [text "RESET"]
        ]
      ]
    ]
  ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \flags -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- ---- MODEL ----


-- type alias Model =
--     {}


-- init : ( Model, Cmd Msg )
-- init =
--     ( {}, Cmd.none )



-- ---- UPDATE ----


-- type Msg
--     = NoOp


-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     ( model, Cmd.none )



-- ---- VIEW ----


-- view : Model -> Html Msg
-- view model =
--     div []
--         [ img [ src "/logo.svg" ] []
--         , h1 [] [ text "Your Elm App is working!" ]
--         ]



-- ---- PROGRAM ----


