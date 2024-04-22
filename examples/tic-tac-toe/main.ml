open Graphics

let init (factor : int) (n : int) : Engine.board =
  let board = Engine.new_board n in
  set_window_title "Tic-Tac-Toe";
  resize_window (2 * n * factor) (2 * n * factor);
  Print.draw_board factor board;
  board

let rec loop (factor : int) (b : Engine.board) (p : Engine.player)
    (cross : Engine.board -> Engine.move) (circle : Engine.board -> Engine.move)
    : unit =
  let m = match p with Engine.Cross -> cross b | Engine.Circle -> circle b in
  let b = Engine.transition b m p in
  Print.draw_board factor b;
  if Engine.has_won b p || Engine.is_block b then
    let _ = wait_next_event [ Button_down ] in
    let b = init factor (Engine.length b) in
    loop factor b p cross circle
  else
    let p = Engine.switch_player p in
    loop factor b p cross circle

let rec interactive_player (factor : int) (b : Engine.board) : Engine.move =
  let s = wait_next_event [ Button_down ] in
  let m =
    match s with
    | { mouse_x; mouse_y; button; keypressed; key } ->
        Print.pos_to_move factor (Engine.length b) mouse_x mouse_y
  in
  if Engine.is_valid b m then m else interactive_player factor b

let () = open_graph ""

let () =
  let n = 3 in
  let factor = 100 in
  let board = init factor n in
  loop factor board Cross
    (interactive_player factor)
    (interactive_player factor)
