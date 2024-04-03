let cell_to_rect ({x;y}:Life.coord) state =
  match state with
  | Life.Dead -> ()
  | Life.Live -> Graphics.draw_rect (x*10) (y*10) 10 10; Graphics.fill_rect (x*10) (y*10) 10 10;;
