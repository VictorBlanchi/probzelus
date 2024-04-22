open Graphics

let n = 3
let board = Engine.new_board n
let factor = 100

let rec loop (factor : int) (b : Engine.board) (p : Engine.player) (s : status)
    : unit =
  match s with
  | { mouse_x; mouse_y; button; keypressed; key } ->
      let m = Print.pos_to_move factor (Engine.length b) mouse_x mouse_y in
      let b = Engine.transition b m p in
      let p = Engine.switch_player p in
      Print.draw_board factor b;
      let s = wait_next_event [ Button_down ] in
      loop factor b p s

let () = open_graph ""

let () =
  set_window_title "Tic-Tac-Toe";
  resize_window (2 * n * factor) (2 * n * factor);
  Print.draw_board factor board;
  loop factor board Cross (wait_next_event [ Button_down ])
