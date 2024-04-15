type player = Cross | Circle
type move = int * int
type mark = player option
type state = mark array array

let is_valid (s : state) (m : move) : bool =
  let i, j = m in
  try s.(i).(j) = None with _ -> false

let assign (a : 'a array) (i : int) (v : 'a) =
  let b = Array.copy a in
  b.(i) <- v;
  b

let transition (s : state) (m : move) (p : player) =
  assert (is_valid s m);
  let i, j = m in
  let new_row = assign s.(i) j (Some p) in
  assign s i new_row

let line (s : state) (i : int) = s.(i)
let row (s : state) (j : int) = Array.map (fun l -> l.(j)) s
let diag1 (s : state) : mark array = Array.mapi (fun i l -> l.(i)) s

let diag2 (s : state) : mark array =
  let n = Array.length s in
  assert (not (n = 0));
  let m = Array.length s.(0) in
  assert (n = m);
  Array.mapi (fun i l -> l.(n - i - 1)) s

let win (s : state) (p : player) : bool =
  let check (t : mark array) = Array.for_all (fun x -> x = Some p) t in
  let n = Array.length s in
  let lines = List.map (line s) (List.init n (fun x -> x)) in
  let rows = List.map (row s) (List.init n (fun x -> x)) in
  let diags = [ diag1 s; diag2 s ] in
  let aligned = List.concat [ lines; rows; diags ] in
  List.exists (fun t -> check t) aligned

let switch_player (p : player) : player =
  match (p : player) with Cross -> Circle | Circle -> Cross
