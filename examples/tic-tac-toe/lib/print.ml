(** This file provides an implementation to print the tic tac toe game *)

open Graphics

let _factor = 10
let factor (x : int) = _factor * x

let draw_cross (x : int) (y : int) =
  set_color blue;
  let recenter () = moveto (factor x) (factor y) in
  List.iter
    (fun (u, v) ->
      recenter ();
      rlineto u v)
    (List.map
       (fun (u, v) -> (factor u, factor u))
       [ (1, 1); (1, -1); (-1, -1); (-1, 1) ])

let draw_circle (x : int) (y : int) =
  set_color red;
  draw_circle (factor x) (y * factor y) _factor

let print_move (p : Engine.player) ((x, y) : Engine.move) : unit =
  moveto x y;
  match p with
  | Engine.Cross -> draw_cross x y
  | Engine.Circle -> draw_circle x y

let print_grid (n : int) : unit =
  let moveto x y : unit = moveto (2 * factor x) (2 * factor y) in
  let lineto x y : unit = lineto (2 * factor x) (2 * factor y) in
  for i = 0 to n + 1 do
    moveto 0 i;
    lineto n i;
    moveto i 0;
    lineto i n
  done

let print_state (s : Engine.state) : unit =
  let n = Array.length s in
  assert (n > 0);
  let m = Array.length s.(0) in
  assert (n = m);
  print_grid n;
  Array.iteri
    (fun i l ->
      Array.iteri
        (fun j m ->
          match m with
          | Some Engine.Cross -> draw_cross (2 * factor i) (2 * factor j)
          | Some Engine.Circle -> draw_circle (2 * factor i) (2 * factor j)
          | None -> ())
        l)
    s
