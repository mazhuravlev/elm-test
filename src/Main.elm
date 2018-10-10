module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Debug exposing (..)
import Browser
import Ttt exposing(checkWin)

type Player = X | O

type Msg = CellClick Int | Reset

type Cell = EmptyCell Int | XCell Int | OCell Int

type State = Game | Tie | Winner Player

type alias Model = {cells: List Cell, currentPlayer: Player, state: State}

-- APP
init: (Model, Cmd Msg)
init = ({cells = List.range 0 8 |> List.map (\x -> EmptyCell x), currentPlayer = X, state = Game}, Cmd.none)

cellView: Bool -> Player -> Cell -> Html Msg
cellView hasWinner player cell =
    let 
      emptyClass = 
        case player of
          X -> "cell cell-x cell-ghost"
          O -> "cell cell-o cell-ghost"
      cellAttrs cellIndex = if hasWinner then [class "cell"] else [class emptyClass, onClick (CellClick cellIndex)]
    in
    case cell of 
      EmptyCell i -> div (cellAttrs i) []
      XCell i -> div [class "cell cell-x"] []
      OCell i -> div [class "cell cell-o"] []

applyClick: Int -> Player -> Cell -> Cell
applyClick cellIndex player =
  let 
    applyer c = 
      case c of
        EmptyCell i ->
          if i /= cellIndex then EmptyCell i else 
            case player of
              X -> XCell i
              O -> OCell i
        XCell i -> XCell i
        OCell i -> OCell i
  in
  applyer

switchPlayer: Player -> Player
switchPlayer player =
   case player of
      X -> O
      O -> X

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let 
    newCells = \cellIndex -> List.map (applyClick cellIndex model.currentPlayer) model.cells
  in
  case msg of
    CellClick cellIndex -> ({
        model | 
        currentPlayer = switchPlayer model.currentPlayer
        ,cells = newCells cellIndex
        ,state = cellIndex |> newCells |> getNextState
        }, Cmd.none)
    Reset -> init

getNextState: List Cell -> State
getNextState cells = 
  let
    isEmpty cell = 
      case cell of
        EmptyCell a -> 1
        _ -> 0 
  in
    case detectWinner cells of
      Nothing -> if (cells |> List.map isEmpty |> List.sum) == 0 then Tie else Game
      Just a -> Winner a

detectWinner: List Cell -> Maybe Player
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
    if checkWin xVec then Just X else if checkWin (List.map omapper cells) then Just O else Nothing

turnToString: Player -> String
turnToString player = 
  case player of
    X -> "X"
    O -> "O"

-- VIEW
view : Model -> Html Msg
view model =
  let
    hasWinner = 
      case model.state of
       Winner a -> True
       _ -> False
    cells = model.cells |> List.map (cellView hasWinner model.currentPlayer)
    msg = 
      case model.state of
        Game -> "Current player: " ++ (turnToString model.currentPlayer)
        Winner X -> "Winner is X!"
        Winner O -> "Winner is O!"
        Tie -> "It's a tie!"
  in
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
