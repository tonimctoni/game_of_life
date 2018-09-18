module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time
import Set

board_size: Int
board_size = 32

pulsar_generator_live_cells: Set.Set (Int,Int)
pulsar_generator_live_cells=
  let
    c=board_size//2
  in
    Set.fromList
    [ (c+1,c+0), (c+0,c+1), (c+1,c+1)
    , (c+1,c-0), (c+0,c-1), (c+1,c-1)
    , (c-1,c+0), (c-0,c+1), (c-1,c+1)
    , (c-1,c-0), (c-0,c-1), (c-1,c-1)
    , (c+0,c+2), (c-0,c-2)
    ]

pentadecathlon_live_cells: Set.Set (Int,Int)
pentadecathlon_live_cells=
  let
    c=board_size//2
  in
    Set.fromList
    [ (c+0,c+0), (c+1,c+0), (c+2,c+0), (c+3,c+0), (c+4,c+0)
    , (c-1,c+0), (c-2,c+0), (c-3,c+0), (c-4,c+0), (c-5,c+0)
    ]

r_pentomino_live_cells: Set.Set (Int,Int)
r_pentomino_live_cells=
  let
    c=board_size//2
  in
    Set.fromList
    [ (c+6,c-5)
    , (c+7,c-4), (c+7,c-5), (c+7,c-6)
    , (c+8,c-4)
    ]


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- MODEL

type alias Model =
  { live_cells: Set.Set (Int,Int)
  , running: Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({live_cells=Set.empty, running=False}, Cmd.none)


-- UPDATE

type Msg
  = Tick Time.Posix
  | ChangeState (Int,Int)
  | DoOneStep
  | RunStop
  | SetPulsarGenerator
  | SetPentadecathlon
  | SetR_Pentomino
  | SetEmpty

game_of_life_iteration: Set.Set (Int,Int) -> Set.Set (Int,Int)
game_of_life_iteration live_cells_no_shadow =
  let
    board_poss =
      List.concat (
        List.map 
          (\y -> (List.map 
            (\x -> (x,y)) 
            (List.range 0 (board_size-1)))
          )
          (List.range 0 (board_size-1))
      )

    bool_to_int: Bool -> Int
    bool_to_int bool=
      case bool of
        True -> 1
        False -> 0

    live_neighbor_count: Set.Set (Int,Int) -> (Int,Int) -> Int
    live_neighbor_count live_cells (x,y)=
      0
      + bool_to_int (Set.member (x-1,y-1) live_cells)
      + bool_to_int (Set.member (x-1,y) live_cells)
      + bool_to_int (Set.member (x,y-1) live_cells)
      + bool_to_int (Set.member (x-1,y+1) live_cells)
      + bool_to_int (Set.member (x+1,y-1) live_cells)
      + bool_to_int (Set.member (x+1,y) live_cells)
      + bool_to_int (Set.member (x,y+1) live_cells)
      + bool_to_int (Set.member (x+1,y+1) live_cells)

    get_live_state: Set.Set (Int,Int) -> (Int,Int) -> Bool
    get_live_state live_cells pos=
      let
        nc = live_neighbor_count live_cells pos
      in
        case Set.member pos live_cells of
          True -> if nc<2 then False else if nc<4 then True else False
          False -> if nc==3 then True else False
  in 
    Set.fromList
    <| List.filter (\pos -> get_live_state live_cells_no_shadow pos) board_poss

kill_if_alive_and_resurrect_if_dead: Set.Set (Int,Int) -> (Int,Int) -> Set.Set (Int,Int)
kill_if_alive_and_resurrect_if_dead live_cells pos =
  case Set.member pos live_cells of
    True -> Set.remove pos live_cells
    False -> Set.insert pos live_cells

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      if model.running
      then ({model | live_cells=game_of_life_iteration model.live_cells}, Cmd.none)
      else (model, Cmd.none)
    ChangeState pos ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=kill_if_alive_and_resurrect_if_dead model.live_cells pos}, Cmd.none)
    DoOneStep ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=game_of_life_iteration model.live_cells}, Cmd.none)
    RunStop ->
      ({model | running=not model.running}, Cmd.none)
    SetPulsarGenerator ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=pulsar_generator_live_cells}, Cmd.none)
    SetPentadecathlon ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=pentadecathlon_live_cells}, Cmd.none)
    SetR_Pentomino ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=r_pentomino_live_cells}, Cmd.none)
    SetEmpty ->
      if model.running
      then (model, Cmd.none)
      else ({model | live_cells=Set.empty}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running
  then Time.every 200 Tick
  else Sub.none

-- VIEW

draw_square : Model -> (Int,Int) -> Html Msg
draw_square model pos=
  div
  [ style "padding" "0pt"
  , style "margin" "1pt"
  , style "width" "10pt"
  , style "height" "10pt"
  , style "float" "left"
  , style "border-radius" "2px"
  , case (Set.member pos model.live_cells) of
      False -> style "background" "#a4dae5"
      True -> style "background" "black"
  , onClick (ChangeState pos)
  ]
  []

draw_square_row: Model -> Int -> Html Msg
draw_square_row model y=
  div [style "float" "top"]
  (List.map (\x -> draw_square model (x,y)) (List.range 0 (board_size-1)))

draw_board: Model -> Html Msg
draw_board model=
  div
    [ style "background" "#b4eaf5"
    , style "border-radius" "4px"
    , style "width" ((String.fromInt (12*board_size))++"pt")
    , style "height" ((String.fromInt (12*board_size))++"pt")
    , style "padding" "2pt"
    ]
    (List.map (\y -> draw_square_row model y) (List.range 0 (board_size-1)))

view : Model -> Browser.Document Msg
view model =
  Browser.Document "Elm Game of Life"
  [ div [style "background" "#B0B0B0", style "top" "0", style "left" "0", style "width" "100%", style "height" "100%", style "position" "absolute"]
    [ div [style "position" "fixed", style "top" "50%", style "left" "50%", style "transform" "translate(-50%, -50%)"]
      [ draw_board model
      , button [onClick RunStop] [text <| if model.running then "Stop" else "Start"]
      , button [onClick DoOneStep, disabled model.running] [text "Step"]
      , div [style "margin-left" "2cm", style "float" "right", style "width" "5cm"]
        [ button [style "float" "top", style "width" "5cm", onClick SetPulsarGenerator, disabled model.running] [text "Set Pulsar Generator"]
        , button [style "float" "top", style "width" "5cm", onClick SetPentadecathlon, disabled model.running] [text "Set Pentadecathlon"]
        , button [style "float" "top", style "width" "5cm", onClick SetR_Pentomino, disabled model.running] [text "Set R-Pentomino"]
        , button [style "float" "top", style "width" "5cm", onClick SetEmpty, disabled model.running] [text "Set Empty"]
        ]
      ]
    ]
  ]
