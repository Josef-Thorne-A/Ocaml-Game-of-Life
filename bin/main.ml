open Gui

let current_board = ref Life.initial_board

let draw_loop board ({button;_}:Graphics.status) =
  if button
  then begin
    Graphics.clear_graph ();
    current_board := Life.step_board board;
    !current_board |> Life.Board.iter cell_to_rect end
  else ()

let () =
  Graphics.open_graph " 400x400";
  Life.Board.iter cell_to_rect Life.initial_board
let _ = Graphics.loop_at_exit [Graphics.Button_down] @@ draw_loop !current_board
