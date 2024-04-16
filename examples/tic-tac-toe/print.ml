(** This file provides an implementation to print the tic tac toe game *)

open Graphics

(** [draw_cross factor x y] draws a cross at position [(x,y)] of size [2*factor] *)
let draw_cross (factor : int) (x : int) (y : int) =
  set_color blue;
  let recenter () = moveto x y in
  List.iter
    (fun (u, v) ->
      recenter ();
      rlineto u v)
    (List.map
       (fun (u, v) -> (factor * u, factor * v))
       [ (1, 1); (1, -1); (-1, -1); (-1, 1) ])

(** [draw_circle factor x y] draws a circle with radius [factor] and center [(x,y)] *)
let draw_circle (factor : int) (x : int) (y : int) =
  set_color red;
  draw_circle x y factor

(** [draw_player factor p (x , y)] draws either a circle or a cross (depending of [p]) at position [(x,y)] *)
let draw_player (factor : int) (p : Engine.player) ((x, y) : Engine.move) : unit
    =
  moveto x y;
  match p with
  | Engine.Cross -> draw_cross x y factor
  | Engine.Circle -> draw_circle x y factor

(** [draw_grid factor n] draws the grid of size [n] *)
let draw_grid (factor : int) (n : int) : unit =
  let moveto x y : unit = moveto (2 * factor * x) (2 * factor * y) in
  let lineto x y : unit = lineto (2 * factor * x) (2 * factor * y) in
  for i = 0 to n do
    moveto 0 i;
    lineto n i;
    moveto i 0;
    lineto i n
  done

(** [draw_state factor s] draws the state [s] of the game*)
let draw_state (factor : int) (s : Engine.state) : unit =
  let n = Array.length s in
  assert (n > 0);
  let m = Array.length s.(0) in
  assert (n = m);
  draw_grid factor n;
  let pos_x i = ((2 * i) + 1) * factor in
  let pos_y j = ((2 * (n - j)) - 1) * factor in
  Array.iteri
    (fun i l ->
      Array.iteri
        (fun j m ->
          match m with
          | Some Engine.Cross -> draw_cross factor (pos_x j) (pos_y i)
          | Some Engine.Circle -> draw_circle factor (pos_x j) (pos_y i)
          | None -> ())
        l)
    s

let () = open_graph ""
let () = draw_state 100 @@ [| [| None; Some Cross |]; [| Some Circle; None |] |]
let _ = wait_next_event []
