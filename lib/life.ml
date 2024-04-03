type state = Dead | Live
type coord = {x : int; y : int}

module Board = Map.Make(struct
    type t = coord
    let compare = compare
  end)
(* val empty_board : 'a Board.t *)
(* val step_board : state Board.t -> state Board.t *)
(* val update_cell : coord -> state -> state Board.t -> state Board.t -> state Board.t *)
let print_board b = Board.iter (fun ({x;y}) m ->
    match m with
      Live -> Printf.printf "Live {%d,%d}" x y
    | Dead -> Printf.printf "Dead {%d,%d}" x y
  ) b

let empty_board = Board.empty;;
let neighbors {x;y} = [
  x,  y+1;
  x+1,y+1;
  x+1,y;
  x+1,y-1;
  x,  y-1;
  x-1,y-1;
  x-1,y;
  x-1,y+1;
] |> List.map (fun (x,y) -> {x;y})

let fold_neighbors board cell ~f ~init =
  neighbors cell |>
  List.fold_left (fun acc n ->
      try f acc (Board.find n board)
      with Not_found -> acc)
    init

let count_live_neighbors =
  fold_neighbors ~init:0 ~f:(fun count nb ->
      match nb with
      | Live -> count + 1
      | Dead -> count)


let next_cell_state board k m =
  let count = count_live_neighbors board k in
  match m with
    Live -> if count = 2 || count = 3 then Live else Dead
  | Dead -> if count = 3 then Live else Dead

let update_cell k m board next_board =
  let () = print_string "\nboard: "; print_board board in
  Board.add k (next_cell_state board k m) next_board

let update_existing_cell board next_board neighbor =
  let () = print_string "\nboard: "; print_board board in
  try let current_state = Board.find neighbor board in
    update_cell neighbor current_state board next_board
  (* Don´t add a Dead node if it didn´t already exist or the grid will grow infinitely *)
  with Not_found ->
  match next_cell_state board neighbor Dead with
  | Dead -> next_board
  | Live -> Board.add neighbor Live next_board

let step_board board =
  let update_neighbors = fun k _ next_board ->
    (* For each neighbor of K, determine its next state and add it to the next board state *)
    neighbors k |> List.fold_left (update_existing_cell board) next_board in
  let update_neighbors_and_cell = fun k m next_board ->
    let res = update_neighbors k m next_board |> update_cell k m board in
    let () = print_string "next_board: "; print_board res; print_string "\n" in res
  in
  Board.fold update_neighbors_and_cell board Board.empty

let initial_board = Board.of_seq (List.to_seq [({x=19;y=20},Live);({x=20;y=20},Live);({x=21;y=20},Live)])
