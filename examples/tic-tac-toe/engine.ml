(** TIC TAC TOE
 This file provides a simple implementation of an engine for tic tac toe game *)

type player = Cross | Circle
type move = int * int
type mark = player option
type board = mark array array

(** [new_board n] returns a new board of size [n*n] *)
let new_board (n : int) : board =
  Array.init n (fun _ -> Array.init n (fun _ -> None))

(** [length b] returns the length of the board [b] and checks if the lengths along the two coordinates are equals*)
let length (b : board) : int =
  let n = Array.length b in
  let m = Array.length b.(0) in
  assert (n = m);
  n

(** [is_valid s m] checks if the move [m] is valid in the board [s] *)
let is_valid (s : board) (m : move) : bool =
  let i, j = m in
  try s.(i).(j) = None with _ -> false

let assign (a : 'a array) (i : int) (v : 'a) =
  let b = Array.copy a in
  b.(i) <- v;
  b

(** [transition s m p] applies the transition where player [p] mark the move [m] in the board [s]*)
let transition (s : board) (m : move) (p : player) =
  assert (is_valid s m);
  let i, j = m in
  let new_row = assign s.(i) j (Some p) in
  assign s i new_row

let line (s : board) (i : int) = s.(i)
let row (s : board) (j : int) = Array.map (fun l -> l.(j)) s
let diag1 (s : board) : mark array = Array.mapi (fun i l -> l.(i)) s

let diag2 (s : board) : mark array =
  let n = Array.length s in
  assert (not (n = 0));
  let m = Array.length s.(0) in
  assert (n = m);
  Array.mapi (fun i l -> l.(n - i - 1)) s

let has_won (s : board) (p : player) : bool =
  let check (t : mark array) = Array.for_all (fun x -> x = Some p) t in
  let n = Array.length s in
  let lines = List.map (line s) (List.init n (fun x -> x)) in
  let rows = List.map (row s) (List.init n (fun x -> x)) in
  let diags = [ diag1 s; diag2 s ] in
  let aligned = List.concat [ lines; rows; diags ] in
  List.exists (fun t -> check t) aligned

let is_block (s : board) : bool =
  Array.for_all
    (fun t ->
      Array.for_all (fun m -> match m with None -> false | Some _ -> true) t)
    s

let switch_player (p : player) : player =
  match (p : player) with Cross -> Circle | Circle -> Cross
