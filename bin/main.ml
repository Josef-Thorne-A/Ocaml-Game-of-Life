open Graphics
let cell_to_rect ({x;y}:Life.coord) state =
  match state with
  | Life.Dead -> ()
  | Life.Live -> draw_rect (x*10) (y*10) 10 10; fill_rect (x*10) (y*10) 10 10;;


let current_board = ref Life.initial_board

let rec draw_loop board (s:Graphics.status) =
  match s with
    {keypressed=true;_} ->
        Graphics.clear_graph ();
        current_board := Life.step_board board;
        !current_board |> Life.Board.iter cell_to_rect;
        Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] |>
        draw_loop !current_board
  | {button=true;mouse_x;mouse_y;_} ->
    current_board := Life.Board.add {x=mouse_x/10;y=mouse_y/10} Life.Live board;
    Graphics.clear_graph ();
    !current_board |> Life.Board.iter cell_to_rect;
    Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] |> draw_loop !current_board
  | _ -> ()
let () =
  Graphics.open_graph " 400x400";
  Life.Board.iter cell_to_rect Life.initial_board

let _ = Graphics.loop_at_exit [Graphics.Button_down;Graphics.Key_pressed] @@ draw_loop !current_board
