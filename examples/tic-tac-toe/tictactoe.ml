open Graphics
open Probzelus
open Ztypes

type pstate = {
  idx : int;  (** particle index *)
  scores : float array;  (** score of each particle *)
}

type prob = pstate

let () =
  open_graph "";
  set_window_title "Tic-Tac-Toe"

(** [restart factor n] restarts a new tic-tac-toe game and returns an empty board *)
let restart (factor : int) (n : int) : Engine.board =
  let board = Engine.new_board n in
  resize_window (2 * n * factor) (2 * n * factor);
  Printer.draw_board factor board;
  board

(** [play_one factor b p m] performs one step of the game, returning the new board. *)
let play_one (factor : int) (b : Engine.board) (p : Engine.player)
    (m : Engine.move) : Engine.board =
  let b = Engine.transition b m p in
  Printer.draw_board factor b;
  if Engine.has_won b p || Engine.is_block b then
    let _ = wait_next_event [ Button_down ] in
    let b = restart factor (Engine.length b) in
    b
  else b

(** An interactive player *)
let rec interactive_player (factor : int) (b : Engine.board) : Engine.move =
  let s = wait_next_event [ Button_down ] in
  let m =
    match s with
    | { mouse_x; mouse_y; button; keypressed; key } ->
        Printer.pos_to_move factor (Engine.length b) mouse_x mouse_y
  in
  if Engine.is_valid b m then m else interactive_player factor b

(** Draw a distribution of move and then wait *)
let draw_move_dist (factor : int) (b : Engine.board) (p : Engine.player)
    (m_dist : Engine.move Distribution.t) : unit =
  Printer.draw_move_dist factor b p m_dist;
  ignore @@ wait_next_event [ Button_down ]